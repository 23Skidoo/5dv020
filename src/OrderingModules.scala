

import scala.collection.immutable.Queue
import scala.collection.immutable.Map


trait OrderingModule[T]{
  def insert(m: IM[T]);
  def get(): T;
  def getAll(): Seq[T];
  def createMessage(d:T): DataMessage[T];
  def updateView(nodes: Seq[Node]);
}


abstract class AbstractOrderingModule[T] extends OrderingModule[T]{
 private var deliveryQueue = Queue[T]();
 def get(): T = {
   val (temp, queue) = deliveryQueue.dequeue;
   deliveryQueue = queue;
   temp;
 }
 def getAll(): Seq[T] = {
   val ret = deliveryQueue.toSeq;
   deliveryQueue = Queue[T]();
   ret
 }
 
 protected def deliver(v: T) = {
   deliveryQueue = deliveryQueue.enqueue(v);
 }
}

class UnorderedQueue[T]() extends AbstractOrderingModule[T]{
  def insert(im: IM[T]){
    deliver(im.dm.d);
  }
  def createMessage(d: T): DataMessage[T] = {
    DM(d);
  }
  def updateView(nodes: Seq[Node]) {}
}

class FIFOQueue[T]() extends AbstractOrderingModule[T]{
  private var holdBacks = Map[Node,List[SeqM[T]]]();
  private var sequences = Map[Node,Int]();
  private var curSeq = 0;
  
  def insert(im: IM[T]){
    im match {
      case IM(m: Message,dm: SeqM[T]) => handle_message(m,dm); 
    }
  }
  private def handle_message(m: Message,dm: SeqM[T]){
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
  
  def createMessage(d:T): DataMessage[T] = {
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
  private var holdBacks = List[IM[T]]();
  private var indexOf = Map[Node,Int]();
  
  private val myAddr = me;
  
  def insert(im: IM[T]){
    im match {
      case IM(m: Message,dm: CausalMessage[T]) => handle_message(im); 
    }
  }
  
  private def handle_message(newIm: IM[T]){
    holdBacks = newIm :: holdBacks
    var changed = true;
    
    while(changed){
      holdBacks.foreach{ im =>
        val index = indexOf(im.m.from);
        val dm = im.dm.asInstanceOf[CausalMessage[T]];
        changed = if(dm.vector(index) == vectorClock(index) + 1 &&
            earlierByOthers(index,vectorClock,dm.vector)){
            deliver(im.dm.d);
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
     val a = for(i <- 0 to myClock.length
        if(i != index && otherClock(i) > myClock(i))) yield{
          false;
        }
     //If the above predicate was true for zero elements, it was earlier than others
     a.length == 0 
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
  
  def createMessage(d:T): DataMessage[T] = {
    val index = indexOf(me);
    vectorClock = vectorClock updated (index, vectorClock( index + 1 ))
    CausalMessage(vectorClock, d);
    
  }
}


class TotalOrderQueue[T](var orderingCallback: () => Int) extends AbstractOrderingModule[T]{
  var holdBacks = Queue[TotalMessage[T]]();
  var currentOrder = 0;
  def insert(im: IM[T]){
    //TODO exception handling
    holdBacks = holdBacks enqueue im.dm.asInstanceOf[TotalMessage[T]]
    
  }
  def createMessage(d: T): DataMessage[T] = {
    TotalMessage(orderingCallback(),d);
  }
  def updateView(nodes: Seq[Node]) {}
}

