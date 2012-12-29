import java.rmi.registry.LocateRegistry
import java.rmi.Remote
import com.twitter.util.Promise
import java.util.concurrent.LinkedBlockingQueue
import scala.collection.mutable.Map
import org.slf4j.LoggerFactory



trait InternalCommunicator[T] extends Receiver{
  def logger = LoggerFactory.getLogger(getClass)
  type ErrorCallback = List[Node] => Unit;
  
  def send(hosts: List[Node], dm: DM[T]): Unit;
  def get(): Message = q.take();
  def poll(): Boolean = !q.isEmpty
  
  private val receivers = Map[Node,Remote]();
  protected val onError: ErrorCallback;
  protected val q = new LinkedBlockingQueue[Message]();
  /*
  protected def hostToRemote(h: Node): Receiver[T] = {
      val r = receivers.get(h);
      r match{
      case Some(x : Receiver[T]) => x;
      case None => {
        val reg = LocateRegistry.getRegistry(h.host, h.port); // TODO What if we get nothing
        val x = reg.lookup(h.name).asInstanceOf[Receiver[T]]; // TODO classcast Exception
        receivers += h -> x;
        x;
      }
      }
  }
  */
  protected class AsyncSender
    (groupMembers: List[Node], m: Message, future: Promise[List[Node]]) extends Runnable{
    def run() = {
        future.setValue(groupMembers.map({ node =>
          try{
            node.ref.recv(m)
            None;
          } catch{
            case e : Exception => {
              logger.error("Node: " + node + " could not recieve " + m)
              Some(node);
            }
          }
          }).flatten);
      }
  }
  
  protected def internal_send(g: List[Node], im: Message) = {
    val failures = new Promise[List[Node]];
    //When failures occur, it will be with a list of failed nodes.
    failures.onSuccess(onError)
    (new Thread(new AsyncSender(g,im,failures))).start();
    
  }
  
}

class BasicCom[T]
  (id:Int, val onError: List[Node] => Unit)
  extends InternalCommunicator[T] with Receiver {
  val me = RefNode(this,id);
  def send(hosts: List[Node],dm: DM[T]){
    internal_send(hosts, Message(NonReliableMessage(me),dm));
  }
  def recv(im: Message){
    logger.info(im.toString);
    q.put(im);
  }
}
 

class ReliableCom[T]
    (val id: Int, val onError: List[Node] => Unit, val hostCallback: () => List[Node])
    extends InternalCommunicator[T] with Receiver{
  val me = RefNode(this,id)
  var rseq = 0;
  val sequences = Map[Node,IntegerSet]();
  def recv(im: Message){ im match {
      //Only deliver reliable messages in a reliable way.
      case Message(rm: ReliableMessage, dm: DataMessage) => {

        if(!sequences.contains(rm.from)){
          sequences += (rm.from -> new IntegerSet());
        }
        if(sequences(rm.from).contains(rm.rseq)){
          //Message already received once, ignore
        }else if(rm.from != me){
          //internal_send(hostCallback(), im)
          q.put(im);
        }
      }
      //If message is not reliable, deliver it directly
      case im: Message => {
        q.put(im)
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
