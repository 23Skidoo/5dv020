import java.rmi.Remote
import com.twitter.util.Promise
import java.util.concurrent.LinkedBlockingQueue
import org.slf4j.LoggerFactory
import java.rmi.RemoteException

/**
 * The common behavior of the communication classes. This class represents
 * the lowest layer in the GroupCom middleware.  
 * @param <T> The type sent inside the messages
 */
trait InternalCommunicator[T] {
  def logger = LoggerFactory.getLogger(getClass)
  
  type ErrorCallback = List[Node] => Unit;
  
  /**
   * Abstract send method, to be defined by inheriting classes
   * This method should call internal_send() which will add a Header
   * and pack it. 
   * @param nodes List of nodes supposed to receive the message 
   * @param dm the message to send
   */
  def send(nodes: List[Node], dm: DM[T]): Unit;

  /** Takes one message from the mailbox, will block if empty.
   * @return the oldest message still in the mailbox.
   */
  def get(): Message = mailBox.take();
  
  /**
   * @return whether or not mailbox is empty
   */
  def poll(): Boolean = !mailBox.isEmpty
  
  /**
   * When sending messages we need a way of signaling failures.
   */
  protected val onError: ErrorCallback;
  protected val mailBox = new LinkedBlockingQueue[Message]();

  /**
   * Runnable created when we send messages to make sending asynchronous.
   * Sets the provided promise with a list of nodes that failed to receive
   * message.
   */
  protected class AsyncSender(
      groupMembers: List[Node],
      m: Message,
      future: Promise[List[Node]] ) extends Runnable{
    def run() = {
        future.setValue(groupMembers.map({ node =>
          try{
            node.ref.recv(m)
            None;
          } catch{
            case e : RemoteException => {
              logger.error("Node: " + node + " could not recieve " + m)
              Some(node);
            }
          }
          }).flatten);
      }
  }
  
  /** This is a basic broadcast to every node provided in g
   * @param g a list of nodes to send to.
   * @param im the message to send.
   */
protected def internal_send(g: List[Node], im: Message) = {
    val failures = new Promise[List[Node]];
    //When failures occur, it will be with a list of failed nodes.
    failures.onSuccess(onError)
    (new Thread(new AsyncSender(g,im,failures))).start();
    
  }
}

/**
 * A communication module that sends messages in a non-reliable fashion.  
 * @param <T> 
 */
class NonReliableCom[T]
    (id:Int, val onError: List[Node] => Unit)
      extends InternalCommunicator[T] with Receiver {
  
  val me = RefNode(NonReliableCom.this, id);
  
  def send(hosts: List[Node],dm: DM[T]){
    internal_send(hosts, Message(NonReliableMessage(me),dm));
  }
  
  def recv(im: Message){
    logger.info(im.toString);
    mailBox.put(im);
  }
}
 
class ReliableCom[T]
    (val id: Int, val onError: List[Node] => Unit, val hostCallback: () => List[Node])
    extends InternalCommunicator[T] with Receiver{
  val me = RefNode(this,id)
  var rseq = 0;
  private var sequences = Map[Node,IntegerSet]();
  def recv(im: Message){ im match {
      //Only deliver reliable messages in a reliable way.
      case Message(rm: ReliableMessage, dm: DataMessage) => {

        if(!sequences.contains(rm.from)){
          sequences += (rm.from -> new IntegerSet());
        }
        if(sequences(rm.from).contains(rm.rseq)){
          //Message already received once, ignore
        }else if(rm.from != me){
          internal_send(hostCallback(), im)
          mailBox.put(im);
        }
      }
      //If message is not reliable, deliver it directly
      case im: Message => {
        mailBox.put(im)
      }
      }
  }

  def send(hosts: List[Node], dm: DM[T]){
    this.synchronized {
    	rseq+=1;
    	internal_send(hosts, Message(ReliableMessage(me,rseq),dm));
    } 
  }

}
