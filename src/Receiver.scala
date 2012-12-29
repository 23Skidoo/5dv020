import java.rmi.Remote
import java.rmi.RemoteException

/**
 * Messages are tuples of Reliable|NonReliable and UserMessage|SystemMessage
 */
case class Message(m:Header,dm:DataMessage)


/**
 * Each message has a header that contains information about the sender
 */
abstract class Header{
  val from: Node;
}

/**
 * Each message carries data
 */
abstract class DataMessage{
  val d: Any;
}

/**
 * A message sent by the ordering modules of GCom, can contain any data.
 */
abstract class OrderedMessage extends DataMessage{
  val d: Any
}

/**
 * A message sent by the system containing different events in the system
 */
case class SystemMessage(d: SystemEvent) extends DataMessage

abstract class SystemEvent;
/**
 * A node wants to join the group, header contains sender
 */
case class JOIN() extends SystemEvent

/**
 * A message that is sent non-reliably
 */
case class NonReliableMessage(from: Node) extends Header;
/**
 * A reliable message, get stamped with a sequence number so we 
 * can check for duplicate messages
 */
case class ReliableMessage(from: Node, rseq: Int) extends Header;

//Ordered messages of different kinds.
case class SeqM[T](seq: Int, d: T) extends OrderedMessage;
case class DM[T](d: T) extends OrderedMessage;
case class CausalMessage[T](vector: Vector[Int], d: T) extends OrderedMessage;
case class TotalMessage[T](order: Int, d: T) extends OrderedMessage;

/**
 * The remote interface exposing the method for receiving messages.  
 */
trait Receiver extends Remote {
  @throws(classOf[RemoteException])
  def recv(m: Message): Unit 
}
