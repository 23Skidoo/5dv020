package gcom.common

/** The node ID is just a (name, host, port) triple. */
sealed case class NodeID (name : String, host: String, port: Int) {

  override def toString() : String = {
    return name + ":" + host + ":" + port.toString;
  }

  /* Since NodeID is a case class, we get correct implementations of
   * equals() and hashCode() for free. */
}

object NodeID {
  def fromString(s : String) : NodeID = {
    val arr = s.split(":");
    if (arr.length != 3)
      throw new IllegalArgumentException("NodeID.toString: can't parse")

    return new NodeID(arr(0), arr(1), arr(2).toInt)
  }
}


/** Used for testing. */
object TestMessage{
  def create(payload: String) = Message(UnreliableMessage(), NoOrdering(), payload);
  def create(payload: String, ordering: MessageOrdering) = Message(UnreliableMessage(), ordering, payload);
}
/** On receiving this, die immediately. */
case class BlackSpot() extends AbstractMessage;

sealed abstract class MessageOrdering;
case class NoOrdering() extends MessageOrdering;
case class CausalMessage(clock: Map[NodeID, Int]) extends MessageOrdering
case class FIFOMessage(seq: Int) extends MessageOrdering
case class TotalOrdering(order: Int) extends MessageOrdering

sealed abstract class Reliability;
case class UnreliableMessage() extends Reliability;
case class ReliableMessage(seq: Int) extends Reliability;

sealed abstract class AbstractMessage extends Serializable {
  var senders = List[NodeID]();

  def getSenders() : List[NodeID] = senders;
  def addSender(n : NodeID) : Unit = senders = n +: senders;
}

case class Message(reliability : Reliability, 
                     ordering : MessageOrdering,
                     payload : String) extends AbstractMessage
/* may add other types of messages to be used by the transport layer,
 * e.g. Ping/Pong/DieImmediately
 */





