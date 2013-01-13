package gcom.ordering;

import gcom.common.NodeID
import gcom.common.Message
import gcom.common.CausalData
import gcom.common.IsCausal
import gcom.communication.Communication

class Causal(
      c: Communication,
      callbck: Message => Unit,
      me: NodeID)
    extends Ordering(c, callbck){

  private var vectorClock = Map[NodeID,Int]();
  private var holdBacks = List[(Message, IsCausal)]();

  private var clock = 0;
  private val myAddr = me;

  def receiveMessage(msg: Message){
    msg match {
      case Message(_, cm : IsCausal, _) => handle_message(msg, cm);
      case msg: Message => callback(msg)
    }
  }

  private def handle_message(newM: Message, newCm: IsCausal){
    holdBacks = (newM, newCm) :: holdBacks
    var changed = true;

    while(changed){
      changed = false;
      holdBacks.foreach{ case (m, cm) =>
        val from = m.senders.head;
        if(cm.clock(from) == vectorClock(from) + 1 &&
            earlierByOthers(from,vectorClock,cm.clock)){
            callback(m);
            holdBacks = holdBacks.filter(_ != m)
            vectorClock = vectorClock + (from -> (vectorClock(from) + 1));
            changed = true
          }

      };
    }
  }

  private def earlierByOthers(
      from: NodeID,
      myClock: Map[NodeID, Int],
      otherClock: Map[NodeID, Int]): Boolean = {
    var earlier = true;
    for((node, clock) <- myClock
      if(node != from && otherClock(node) > myClock(node))) {
         earlier = false;
    }
     //If the above predicate was true for zero elements, it was earlier than
     //others
     earlier
  }

  /** * @param nodes is a sequence of tuples containing nodes and their current
  clock */

  def updateView(newClock: Map[NodeID, Int]) = {
    //Only remember messages from nodes in view
    holdBacks = holdBacks.filter({case (m,cm) =>
                                  newClock.contains(m.senders.head) })
    //Remove clocks from messages
    holdBacks = holdBacks.map { case (m, cm) =>
      val clocks = cm.clock -- newClock.keySet
      (m,cm.updated(clocks))
    }
    vectorClock = newClock
  }

  def createOrdering(): CausalData = {
    val index = (me);
    clock = clock + 1;
    CausalData(vectorClock + (index -> clock ));
  }
}

object Causal {
  def create(t : Communication,
             callbck : Message => Unit, thisNode: NodeID) : Causal = {
    val ord = new Causal(t, callbck,thisNode)
    t.setOnReceive(ord.receiveMessage)
    return ord
  }
}
