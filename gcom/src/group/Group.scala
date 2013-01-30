package gcom.group;

import java.rmi.Remote
import java.rmi.RemoteException
import java.rmi.registry.LocateRegistry
import java.rmi.registry.Registry
import java.rmi.server.UnicastRemoteObject

import org.slf4j.Logger

import scala.collection.mutable.Stack
import scala.collection.concurrent.TrieMap

import gcom.NameServer
import gcom.Group
import gcom.common._
import gcom.communication.Communication
import gcom.ordering.Ordering
import gcom.Communicator

// Dummy group: Stores all data on the name server.

class DummyGroup (grp : Group, lggr : Logger, nsrv : NameServer,
                  ndID : NodeID, ord : Ordering,
                  callbck : String => Unit)
      extends Communicator {
  val group        = grp
  val nameserver   = nsrv
  val ordering     = ord
  val nodeID       = ndID
  var callback     = callbck
  val logger       = lggr

  def incCounter() : Int = { nsrv.incCounter() }
  def broadcastMessage(msg : String) : Unit = {
    val nodes = nsrv.listGroupMembers()
    ordering.sendToAll(nodes, msg)
  }
  def listGroupMembers() : Set[NodeID] = { nameserver.listGroupMembers() }
  // Not implemented with dummy group mngmnt.
  def leaveGroup() : Unit = {
    logger.debug("DummyGroup.leaveGroup: not implemented"); }
  def killGroup() : Unit = {
    logger.debug("DummyGroup.killGroup: not implemented"); }

  def isLocked() : Boolean = false

  // Boilerplate.
  def setOnReceive(callbck : String => Unit) = { callback = callbck; }
  def receiveMessage(msg : Message) = {
    msg match {
      case Message(_, _, payload) => callback(payload)
    }
  }
}

object DummyGroup {
  def create(grp : Group, lggr : Logger, nsrv : NameServer, ndID: NodeID,
             comm : Communication, ord : Ordering,
             callbck : String => Unit)
              : Communicator = {
    nsrv.getOrSetGroupLeader(grp, ndID)
    val dummy = new DummyGroup(grp, lggr, nsrv, ndID, ord,
                               callbck)
    comm.setHostCallback(dummy.listGroupMembers)
    ord.setOnReceive(dummy.receiveMessage)
    ord.setOrderCallback(dummy.incCounter)
    nsrv.joinGroup(ndID)
    return dummy
  }
}

/* Non-dummy group: uses 2PC to agree on a sequence of commands. */

// Group state.
sealed case class GroupState
(leader : NodeID, members : Map[NodeID, Int],
 isLocked : Boolean, // Is the group locked?
 msgCounter : Int, // counter for the total order
 opCounter : Int,  // # of the last agreed-upon GroupStateOp
 nodeCounter : Int) // # of the last joined node

/* Group state ops. */
sealed abstract class GroupStateOp extends Serializable
case class JoinGroup(node : NodeID) extends GroupStateOp;
case class LeaveGroup(nodes : Set[NodeID]) extends GroupStateOp;
case class UnlockGroup() extends GroupStateOp;
case class IncCounter() extends GroupStateOp;
case class NewLeader(n : NodeID) extends GroupStateOp;

// Remote interface for 2PC. Not using the communication layer for this because
// to avoid delayed/dropped msgs. We pretend that a reliable protocol (e.g. TCP)
// is used for consensus and an unreliable one (e.g. UDP) for passing messages
// around.
trait GroupCommunication extends Remote {
  // Called by a new node that wants to join.
  @throws(classOf[RemoteException])
  def letMeIn(who : NodeID) : Option[GroupState]

  // 2PC: prepare.
  @throws(classOf[RemoteException])
  def consensusPrepare(from : NodeID, n: Int, op : GroupStateOp) : Boolean

  // 2PC: commit.
  @throws(classOf[RemoteException])
  def consensusCommit(from : NodeID, n: Int, op : GroupStateOp) : Unit

  // 2PC: abort.
  @throws(classOf[RemoteException])
  def consensusAbort(from : NodeID, sn: Int, op : GroupStateOp) : Unit

  @throws(classOf[RemoteException])
  def ping() : Unit;
}

class BasicGroup (grp: Group, lggr : Logger, nsrv : NameServer,
                  ndID : NodeID, ord : Ordering,
                  callbck : String => Unit)
      extends GroupCommunication with Communicator {
  val group        = grp
  val nameserver   = nsrv
  val ordering     = ord
  val nodeID       = ndID
  var callback     = callbck
  val logger       = lggr
  val maxNodes     = grp.num
  // TOFIX: Make an Option[GroupState]
  @volatile
  var state : GroupState = null
  var history            = Stack[(Int, GroupStateOp)]()

  // Copy-paste from Transport. TOFIX: remove duplication.
  val registries = new TrieMap[(String, Int), Registry]();
  val stubs      = new TrieMap[NodeID, GroupCommunication]();

  /* If we can't locate registry, dying with an exception is fine. */
  private def locateRegistry(host : String, port : Int) : Registry = {
    registries.get((host, port)) match {
      case None => { val registry = LocateRegistry.getRegistry(host, port);
                     registries((host, port)) = registry;
                     return registry;
                   }
      case Some(r) => r
    }
  }

  /* It's OK to throw exceptions here: can't locate stub - we're toast. */
  private def locateStub(n : NodeID) : Option[GroupCommunication] = {
    stubs.get(n) match {
      case None => {
        val registry = locateRegistry(n.host, n.port)
        val stub = registry.lookup(n.name + "-group")
                   .asInstanceOf[GroupCommunication]
        stubs(n) = stub
        Some(stub)
      }
      case s    => s
    }
  }

  private def register() = {
    val stub = UnicastRemoteObject.exportObject(this, 0)
    val registry = locateRegistry(nodeID.host, nodeID.port)
    registry.rebind(nodeID.name + "-group", stub)
    logger.debug("Group communication ready")
  }

  def ping : Unit = { ; }

  def pingNode(dst : NodeID) : Boolean = {
    try {
      val mstub = locateStub(dst)
      mstub.map { stub =>
        stub.ping(); true;
      }.getOrElse(false)
    }
    catch {
      case _ : RemoteException => false
    }
  }

  // Initialization.
  def joinGroup() : Unit = {
    val leader = nsrv.getOrSetGroupLeader(grp, ndID)
    
    //Decide whether this should be a locked group or not
    if (leader == nodeID && maxNodes == 0) {
      state = new GroupState(nodeID, Map(nodeID -> 0), false, 0, 0, 0)
    } else if(leader == nodeID){
      state = new GroupState(nodeID, Map(nodeID -> 0), true, 0, 0, 0)
    }
    else {
      val stub = locateStub(leader).get
      
      stub.letMeIn(nodeID) match {
        case None => {
          logger.debug("They didn't let me in! (shouldn't happen)")
          throw new RuntimeException("joinGroup failed!")
        }
        case Some(s) => {
          logger.debug("They let me in! Praise the gods!")
          state = s
        }
      }
    }
  }

  // Let a new node join the group.
  def letMeIn(who : NodeID) : Option[GroupState] = {
    if (state == null || state.members.contains(who) ||
        // If we have a limit and now are unlocked allow no one
        (maxNodes > 0 && !state.isLocked) ) {
      return None;
    }
    else {
      return updateSharedState(JoinGroup(who))
    }
  }

  /* 2PC impl. */
  var next2PCAction : Option[(NodeID, Int, GroupStateOp)] = None

  // 2PC: prepare.
  def consensusPrepare(from : NodeID, num : Int, op : GroupStateOp) : Boolean = {
    if (state == null)
      return false;
    if (num != state.opCounter + 1)
      return false;
    next2PCAction match {
      case Some(t@(frm, n, o)) => {
        if (t == (from, num, op)) true
        else { if (!pingNode(frm)) { next2PCAction = Some(from, num, op); true}
               else { false } }
      }
      case None => { next2PCAction = Some(from, num, op); true }
    }
  }

  // 2PC: commit.
  def consensusCommit(from : NodeID, num : Int, op : GroupStateOp) : Unit = {
    if (state == null)
      return;

    next2PCAction match {
      case Some(t@(frm, n, o)) => {
        if (t == (from, num, op)) {
          performGroupStateOp(num, op)
          next2PCAction = None
        }
        else  {
          logger.debug("consensusCommit: Asked to commit a wrong op.")
          assert(false)
        }
      }
      case None => {
        logger.debug("consensusCommit: commit w/o prepare.")
        assert(false)
      }
    }
  }

  // 2PC: abort.
  def consensusAbort(from : NodeID, num : Int, op : GroupStateOp) : Unit = {
    if (state == null)
      return;

    next2PCAction match {
      case Some(t@(frm, n, o)) => {
        if (t == (from, num, op)) { next2PCAction = None }
        else { ; }
      }
      case None => { ; }
    }
  }

  // Consensus.

  private def updateSharedState(op : GroupStateOp) : Option[GroupState] = {
    import scala.util.control.Breaks._
    lggr.debug("Updating shared state with " + op)

    var prepared = false
    var committed = false
    while (!committed) {
      val newOpCounter = state.opCounter + 1
      val groupMembers = state.members.keySet

      breakable {
        for (member <- groupMembers) {
          val mstub = locateStub(member)
          // Using try, locateStub does not guarantee stub is alive
          try{
        	  prepared = mstub.get.consensusPrepare(nodeID, newOpCounter, op)
        	  if (!prepared) {
        	    break; 
        	  }
          } catch {
            case e : RemoteException => {
              logger.debug("updateSharedState prepare: node "
                  + member + "  missing. " + e)
            }
          }
        }
      }
      for (member <- groupMembers) {
        val mstub = locateStub(member)
        try{
          val stub = mstub.get
          if(prepared)
            stub.consensusCommit(nodeID, newOpCounter, op)
          else
            stub.consensusAbort(nodeID, newOpCounter, op)
        } catch {
          case se : RuntimeException => {
              logger.debug("Could not commit state: " + se )
        	  return None
          }
          case e : RemoteException => {
            logger.debug("updateSharedState commit: node " + member + " missing. " + e )
          }
        }
      }
      
      if (prepared) committed = true
      if (!committed) {
       lggr.debug("Could not reach consensus, retrying in 0.5s")
       Thread.sleep(500) // hacky 
      }
    }
    return Some(state)
  }

  private def electNewLeader(notMe : Boolean) : Unit = {
    try {
      var done = false
      while (!state.members.isEmpty && !done) {
        val membersFiltered = if (notMe) { state.members.filter(_._1 != nodeID) }
                              else { state.members }
        if (!membersFiltered.isEmpty) {
          val candidateLeader = membersFiltered.minBy(_._2)._1
          if (pingNode(candidateLeader)) {
            updateSharedState(NewLeader(candidateLeader))
            nameserver.setGroupLeader(group, candidateLeader)
            done = true
          }
          else {
            updateSharedState(LeaveGroup(Set(candidateLeader)))
          }
        }
        else {
          // Group empty, we're shutting down.
          nameserver.removeGroup(group)
          done = true
        }
      }
    }
    catch {
      // Can't reach the nameserver: network failure, we're toast.
      case e : RemoteException => killGroup()
    }
  }

  // Called after consensus has been reached.
  private def performGroupStateOp(num : Int, op : GroupStateOp) {
    state.synchronized {
      
    // Make sure a locked group does not allow too many nodes
    op match {
      case _ : JoinGroup => {
        if( maxNodes != 0 && !state.isLocked ){
          // TODO create more specific exception
          throw new RuntimeException("You are not allowed to join!")
        }
      }
      case _ => {}
    }
    
    logger.debug("performGroupStateOp( " + num.toString
                 + ", " + op.toString + " )")

    state = state.copy(opCounter = state.opCounter + 1)
    assert(state.opCounter == num)
    history.push((num, op))

    op match {
      case JoinGroup(node) =>
        { val newNodeCounter = state.nodeCounter + 1
          state = state.copy(members = state.members + (node -> newNodeCounter),
                             nodeCounter = newNodeCounter);
          /* If this group is static (maxNodes is bigger than zero) and
           * we are reaching maximum size, "unlock" the group so sending
           * can be started.
           */
          if(maxNodes > 0 && state.members.size == maxNodes){
            assert(state.isLocked == true)
            state = state.copy(isLocked = false)
          }
        }
      case LeaveGroup(nodes) =>
        { state = state.copy(members = state.members -- nodes); }
      case IncCounter() => state = state.copy(msgCounter = state.msgCounter + 1);
      case NewLeader(n) => state = state.copy(leader = n);
      case UnlockGroup()  => state = state.copy(isLocked = false);
    }
    }

    op match {
      case LeaveGroup(s) => {
        if (s.contains(nodeID)) {
          if (nodeID == state.leader) {
            nameserver.removeGroup(group);
          }
          publish(TimeToDie())

          state.synchronized {
            state = null
          }
        }
        else {
          publish(UpdateGroupMembers(this.listGroupMembers()))
        }
      }
      case JoinGroup(n) => {
        assert(maxNodes == 0 || state.members.size <= maxNodes)
        publish(UpdateGroupMembers(this.listGroupMembers()))
        if(!state.isLocked){
       	  publish(GroupUnlocked())
        }
      }
      case UnlockGroup() => {
        assert(true == false)
      }
      case _ => ;
    }
  }

  // Required ops.
  def incCounter() : Int = {
    val newState = updateSharedState(IncCounter())
    assert(!newState.isEmpty)
    return state.msgCounter
  }
  def broadcastMessage(msg : String) : Unit = {
    lggr.debug("Received new message from application layer")
    
    if(state.isLocked){
      lggr.debug("broadCastMessage: Tried to send before ready")
      return //TODO Maybe cache messages, or something
    }
    
    val membersSet = this.listGroupMembers()
    val reachable = ordering.sendToAll(membersSet, msg)
    val unreachable = membersSet.diff(reachable);
    lggr.debug("Following nodes could not be reached" + unreachable)
    if (!unreachable.isEmpty) {
      updateSharedState(LeaveGroup(unreachable))
    }
    // State could be null if the group was locked and we're now shutting down.
    if (state != null && unreachable.contains(state.leader)) {
      electNewLeader(false)
    }
  }

  def listGroupMembers() : Set[NodeID] = state.members.keySet
  def leaveGroup() : Unit = {
    if (state != null) {
      val iWasLeader = if (state.leader == nodeID) true else false
      if (iWasLeader)
        electNewLeader(true)
      updateSharedState(LeaveGroup(Set(nodeID)));
      state = null
    }
  }
  def killGroup() : Unit = {
    updateSharedState(LeaveGroup(this.listGroupMembers()))
    nameserver.removeGroup(group);
    state = null
  }

  def isLocked() : Boolean = state.isLocked

  // Boilerplate.
  def setOnReceive(callbck : String => Unit) = { callback = callbck; }
  def receiveMessage(msg : Message) = {
    msg match {
      case Message(_, _, payload) => callback(payload)
    }
  }
}

object BasicGroup {
  def create(grp : Group, lggr : Logger, nsrv : NameServer, ndID: NodeID,
             comm : Communication, ord : Ordering,
             callbck : String => Unit)
            : BasicGroup = {
    val ret = new BasicGroup(grp, lggr, nsrv, ndID, ord, callbck)
    ret.register()
    ret.joinGroup()

    comm.setHostCallback(ret.listGroupMembers)
    ord.setOnReceive(ret.receiveMessage)
    ord.setOrderCallback(ret.incCounter)

    return ret
  }
}
