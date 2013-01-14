package gcom.ordering;

import gcom.common._
import gcom.communication.Communication


class FIFO(c: Communication, callbck: Message => Unit)
      extends Ordering(c, callbck){

  private var holdBacks = Map[NodeID,List[(Message, FIFOData)]]();
  private var sequences = Map[NodeID,Int]();
  private var curSeq = 0;

  def receiveMessage(msg : Message) {
    msg match {
      case Message(_, fm: FIFOData, _) => handle_message(msg,fm);
      case m : Message => callback(m)
    }
    val hold = holdBacks.map( {
      case (node, list) => {
        ("" + node + " " + sequences(node)) :: list.map("" + _)
      } 
    })
    publish(UpdateQueue(this,"FIFO: " + curSeq, hold.flatten.toList))
     
  }

  private def handle_message(newm: Message, newfm: FIFOData){
    val from = newm.senders.head
    if(holdBacks.get(from).isEmpty){
      holdBacks += (from -> List());
    }
    if(sequences.get(from).isEmpty){
      sequences += (from -> newfm.seq);
    }
    if(sequences(from) > newfm.seq){
      //Discard message
    } else if(sequences(from) == newfm.seq){
      sequences += from -> (newfm.seq + 1);
      callback(newm)
      check_queue(from);
    } else {
      holdBacks += from-> ((newm,newfm) :: holdBacks(from));
    }
  }

  private def check_queue(host: NodeID){
    var sequence = sequences(host)
    var list = holdBacks(host).sortBy(m => m._2.seq);
    while(list.nonEmpty && sequence == list.head._2.seq){
      callback(list.head._1)
      sequence += 1;
      list = list.tail;
    }
    holdBacks += (host -> list);
    sequences += (host -> sequence);
  }

  def createOrdering(): FIFOData = {
    val msg = FIFOData(curSeq);
    curSeq += 1;
    return msg;
  }

  def setOrderCallback( callback: () => Int ) = { ; }
}

object FIFO{
  def create(t: Communication, callbck: Message => Unit) : FIFO = {
    val fifo = new FIFO(t, callbck)
    t.setOnReceive(fifo.receiveMessage)
    fifo
  }
}
