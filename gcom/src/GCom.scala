package gcom;

import java.rmi.Remote
import java.rmi.RemoteException

import scala.swing.Publisher

import gcom.common.NodeID

/** Client interface for the GCom middleware. */

sealed abstract class MulticastType;
case class UnreliableMulticast() extends MulticastType;
case class ReliableMulticast() extends MulticastType;

sealed abstract class OrderingType;
case class NoOrdering() extends OrderingType;
case class FIFOOrdering() extends OrderingType;
case class CausalOrdering() extends OrderingType;
case class TotalOrdering() extends OrderingType;
case class CausalTotalOrdering() extends OrderingType;

sealed case class Group (name : String
                         , multicast : MulticastType
                         , ordering : OrderingType, num: Int)

trait NameServer extends Remote {

  @throws(classOf[RemoteException])
  def listGroups() : Set[Group]

  @throws(classOf[RemoteException])
  def getGroupLeader(g : Group) : Option[NodeID]

  @throws(classOf[RemoteException])
  def setGroupLeader(g : Group, l : NodeID) : Unit

  @throws(classOf[RemoteException])
  def getOrSetGroupLeader(g : Group, l : NodeID) : NodeID

  @throws(classOf[RemoteException])
  def removeGroup(g: Group) : Boolean

  /* Used by DummyGroup. */
  @throws(classOf[RemoteException])
  def joinGroup(n : NodeID) : Unit

  /* Used by DummyGroup. */
  @throws(classOf[RemoteException])
  def listGroupMembers() : Set[NodeID]

  /* Used by DummyGroup. */
  @throws(classOf[RemoteException])
  def incCounter() : Int

  /* For tests. */
  @throws(classOf[RemoteException])
  def clearGroups() : Unit
}

trait Communicator extends Publisher {
  // Send/receive messages.
  def broadcastMessage(msg : String) : Unit
  def setOnReceive(callback : String => Unit)

  // Group operations.
  def listGroupMembers() : Set[NodeID]
  def leaveGroup() : Unit
  def killGroup() : Unit

  // Increment the shared group counter.
  def incCounter() : Int

  // Locking the group
  def isLocked() : Boolean
}
