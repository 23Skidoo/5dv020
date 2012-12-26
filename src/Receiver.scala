

import java.rmi.Remote
import java.rmi.RemoteException


/*Cases for message module*/
abstract class Message{
  val from: Node;
}
case class SimpleMessage(from: Node) extends Message;
case class ReliableMessage(from: Node, rseq: Int) extends Message;

case class IM[T](m:Message,dm:DataMessage[T])

/*
class SeqMessage[T](val seq: Int, d: T) extends DataMessage[T]{
  var from = Host("Pleasing the compiler",1,"Never allow me to be null");
  val data = d;
};*/

case class SeqM[T](seq: Int, d: T) extends DataMessage[T];
case class DM[T](d: T) extends DataMessage[T];
case class CausalMessage[T](vector: Vector[Int], d: T) extends DataMessage[T];
case class TotalMessage[T](order: Int, d: T) extends DataMessage[T];
abstract class DataMessage[T]{
  val d: T;
}
/*
Cases for order module*/

/*case class SystemMessage(data: Any) extends DataMessage;
case class UnorderedMessage(data: Any) extends DataMessage;


case class OrderedMessage(order: Int, data: Any) extends DataMessage;
*/
trait Receiver[T] extends Remote {
  @throws(classOf[RemoteException])
  def recv(m : IM[T]) : Unit 
}
