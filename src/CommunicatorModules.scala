import java.rmi.registry.LocateRegistry
import java.rmi.Remote
import com.twitter.util.Promise
import java.util.concurrent.LinkedBlockingQueue
import scala.collection.mutable.Map
import org.slf4j.LoggerFactory



trait InternalCommunicator[T]{
  def logger = LoggerFactory.getLogger(getClass)
  type ErrorCallback = (Node) => Unit;
  
  def send(hosts: List[Node], dm: DM[T]): Unit;
  def get(): IM[T] = q.take();
  def poll(): Boolean = !q.isEmpty
  
  private val receivers = Map[Node,Remote]();
  protected val onError: ErrorCallback;
  protected val q = new LinkedBlockingQueue[IM[T]]();
  
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
  protected class AsyncSender
    (g: List[Node],m: IM[T],future: Promise[List[Node]]) extends Runnable{
    def run() = {
        future.setValue(g.map({ h =>
        try{
          hostToRemote(h) recv(m)
          None;
        } catch{
        case e : Exception => {
            onError(h)
            //TODO Log exception
            Some(h);
          }
        }
        }).flatten);
      }
  }
  
  protected def internal_send(g: List[Node], im: IM[T]) = {
    val future = new Promise[List[Node]];
    (new Thread(new AsyncSender(g,im,future))).start();
    future;
  }
  
}


class BasicCom[T]
  (val me:Node, val onError: Node => Unit)
  extends InternalCommunicator[T] with Receiver[T] {
  
  def send(hosts: List[Node],dm: DM[T]){
    internal_send(hosts, IM(SimpleMessage(me),dm));
  }
  def recv(im: IM[T]){
    logger.info(im.toString);
    q.put(im);
  }
}
 

class ReliableCom[T]
    (val me:Node, val onError: Node => Unit, val hostCallback: () => List[Node])
    extends InternalCommunicator[T] with Receiver[T]{
  var rseq = 0;
  val sequences = Map[Node,IntegerSet]();
  def recv(im: IM[T]){
    im match {
      case IM(rm: ReliableMessage, dm: DataMessage[T]) => {

        if(!sequences.contains(rm.from)){
          sequences += (rm.from -> new IntegerSet());
        }
        if(sequences(rm.from).contains(rm.rseq)){
          //Message already received once, ignore
        }else if(rm.from != me){
          internal_send(hostCallback(), im)
          q.put(im);
        }
      }
      }
  }

  def send(hosts: List[Node], dm: DM[T]){
    this.synchronized {
    	rseq+=1;
    	internal_send(hosts, IM(ReliableMessage(me,rseq),dm));
    } 
  }

}
