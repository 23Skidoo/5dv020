import scala.collection.immutable.Queue
import scala.collection.immutable.Map
import org.slf4j.LoggerFactory


/**
 * Modules for ordering incoming system messages.
 * Needs to be constructed so they at first only include themselves.
 * @param <T>
 */
//Could be altered to have a callback on deliver instead.

//None of the implemented orderings can properly handle updateView

trait OrderingModule[T]{
  def insert(m: Message);
  def get(): T;
  def getAll(): Seq[T];
  def createMessage(d:T): DataMessage;
  def updateView(nodes: Seq[Node]); //Don't think this is possible to do in one common way!
}

/**
 * Common behavior between the ordering modules
 */
abstract class AbstractOrderingModule[T] extends OrderingModule[T]{
 private var deliveryQueue = Queue[T]();
 
 /**
 * @return the next item from the queue;
 */
def get(): T = {
   val (temp, queue) = deliveryQueue.dequeue;
   deliveryQueue = queue;
   temp;
 }

 /**
 * @return all items available for retrieval
 */
def getAll(): Seq[T] = {
   val ret = deliveryQueue.toSeq;
   deliveryQueue = Queue[T]();
   ret
 }
 
 /**
 * @param value to deliver to userspace  
 */
protected def deliver(value: T) = {
   deliveryQueue = deliveryQueue.enqueue(value);
 }
}

/**
 * A Queue that does nothing to order the incoming elements
 
 * @param <T>
 */
class UnorderedQueue[T]() extends AbstractOrderingModule[T]{
  def insert(im: Message){
    deliver(im.dm.asInstanceOf[DM[T]].d);
  }
  def createMessage(d: T): DataMessage = {
    DM(d);
  }
  def updateView(nodes: Seq[Node]) {}
}

/** A Queue delivering each message in the order they were sent by 
 * that specific node.
 * @param <T> The type to store inside this ordering module
 */
class FIFOQueue[T]() extends AbstractOrderingModule[T]{
  
  //TODO What will new nodes do? They have no knowledge of earlier sequence numbers... 
  // 1. Assume first message they receive is the first in the sequence (bad)
  // 2. Implement some way to ask that specific node.
  // 3. When node joins, send a sequence number to him
  private var holdBacks = Map[Node,List[SeqM[T]]]();
  private var sequences = Map[Node,Int]();
  private var curSeq = 0;
  
  def insert(im: Message){
    im match {
      case Message(m: Header,sm: SeqM[T]) => handle_message(m,sm); 
    }
    
  }
  private def handle_message(m: Header,dm: SeqM[T]){
    if(holdBacks.get(m.from).isEmpty){
      holdBacks += (m.from -> List[SeqM[T]]());
    }
    if(sequences.get(m.from).isEmpty){
      sequences += (m.from -> 0);
    }
    if(sequences(m.from) == dm.seq){
      sequences += m.from -> (dm.seq + 1);
      deliver(dm.d);
      check_queue(m.from);
    }
    else {
      holdBacks += m.from-> (dm :: holdBacks(m.from));  
    }
  }
  
  private def check_queue(host: Node){
    var sequence = sequences(host)
    var list = holdBacks(host).sortBy(m => m.seq);
    while(list.nonEmpty && sequence == list.head.seq){
      deliver(list.head.d);
      sequence += 1;
      list = list.tail;
    }
    holdBacks += (host -> list);
    sequences += (host -> sequence);
  }
  
  def createMessage(d:T): DataMessage = {
    val msg = SeqM(curSeq,d);
    curSeq += 1;
    return msg;
  }
  
  def updateView(nodes: Seq[Node]) = {
    val newHoldBacks = for (n <- nodes) yield {
      if(holdBacks.contains(n)) { (n, holdBacks(n)    ) }
      else                      { (n, List[SeqM[T]]() ) }
    };
    holdBacks = Map(newHoldBacks: _*);
    
    val updatedSeqs = for(n <- nodes) yield {
      if(sequences.contains(n)) { (n, sequences(n) )}
      else                      { (n, 0            )}
    }
    sequences = Map(updatedSeqs: _*);
  }
}

class CausalQueue[T](me: Node) extends AbstractOrderingModule[T]{
  
  private var vectorClock = Vector[Int]();
  private var holdBacks = List[Message]();
  private var indexOf = Map[Node,Int]();
  
  private val myAddr = me;
  
  def insert(im: Message){
    im match {
      case Message(m: Header,dm: CausalMessage[T]) => handle_message(im); 
    }
  }
  
  private def handle_message(newIm: Message){
    holdBacks = newIm :: holdBacks
    var changed = true;
    
    while(changed){
      holdBacks.foreach{ im =>
        val index = indexOf(im.m.from);
        val dm = im.dm.asInstanceOf[CausalMessage[T]];
        changed = if(dm.vector(index) == vectorClock(index) + 1 &&
            earlierByOthers(index,vectorClock,dm.vector)){
            deliver(dm.d);
            vectorClock = vectorClock updated (index,vectorClock(index) + 1);
            true
          }
          else false
      };
    }
  }

  private def earlierByOthers(
      index: Int, 
      myClock: Vector[Int], 
      otherClock: Vector[Int]): Boolean = {
    var wasEarlier = true;
    for(i <- 0 to myClock.length
        if(i != index && otherClock(i) > myClock(i))) {
          wasEarlier = false;
        }
     wasEarlier
  }
  
    def updateView(nodes: Seq[Node]) = {
      //Only remember messages from nodes in view
      holdBacks = holdBacks.filter({holdBack => nodes.contains(holdBack.m.from) })
      
      // for the new nodes, check if we have their index, if not initialize to zero
      val seq = (nodes map (indexOf.get(_))) map (_ match {
        case None => 0;
        case Some(x: Int) => vectorClock(x);
      })
      vectorClock = Vector(seq: _*);

      //Assume nodes index are their index in the view
      indexOf = Map(nodes zipWithIndex: _*)
      
      
    }
  
  def createMessage(d:T): DataMessage = {
    val index = indexOf(me);
    vectorClock = vectorClock updated (index, vectorClock( index + 1 ))
    CausalMessage(vectorClock, d);
    
  }
}


class TotalOrderQueue[T](var orderingCallback: () => Int) extends AbstractOrderingModule[T]{
  var holdBacks = Queue[TotalMessage[T]]();
  var currentOrder = 0;
  def insert(im: Message){
    //TODO exception handling
    holdBacks = holdBacks enqueue im.dm.asInstanceOf[TotalMessage[T]]
    
  }
  def createMessage(d: T): DataMessage = {
    TotalMessage(orderingCallback(),d);
  }
  def updateView(nodes: Seq[Node]) {}
}

