package gcom.ordering;

import gcom.common.NodeID
import gcom.common.Message
import gcom.common.CausalData
import gcom.common.IsCausal
import gcom.communication.Communication
import gcom.common.UpdateQueue

class Causal(
      c: Communication,
      callbck: Message => Unit,
      me: NodeID)
    extends Ordering(c, callbck){

  private var vectorClock = Map[NodeID,Int]();
  private var holdBacks = List[(Message, IsCausal)]();

  private var clock = 0;
  private val myAddr = me;

  def receiveMessage(msg: Message){
    msg match {
      case Message(_, cm : IsCausal, _) => handle_message(msg, cm);
      case msg: Message => callback(msg)
    }
    publish(UpdateQueue(
        this,
        "Causal: " + clock,
        "Our clock " + vectorClock :: holdBacks map (_.toString) ))
  }

  private def handle_message(newM: Message, newCm: IsCausal){
    val from = newM.senders.head
    //If we haven't seen a node before, assume this is the first we get
    var newVectors = (newCm.clock -- vectorClock.keys)
    //If this was sent from one of the new ones, decrement so it gets delivered
    if(newVectors.contains(from)){
      val clock = newVectors(from) - 1
      newVectors = newVectors + (from -> clock)
    }
    vectorClock = newVectors ++ vectorClock
    
    
    
  
    var changed = true;
    // If message is from the past, ignore
    if(vectorClock(from) > newCm.clock(from)){
      changed = false
    } else {
      holdBacks = (newM, newCm) :: holdBacks
    }
    
    while(changed){
      changed = false;
      holdBacks.foreach{ case (m, cm) =>
        val from = m.senders.head;
        if(cm.clock(from) == vectorClock(from) + 1 &&
            earlierByOthers(from,vectorClock,cm.clock)){
            callback(m);
            holdBacks = holdBacks.filter(_ != (m, cm))
            vectorClock = vectorClock + (from -> (vectorClock(from) + 1));
            changed = true
          }

      };
    }
  }
 
  private def earlierByOthers(
      from: NodeID,
      myClock: Map[NodeID, Int],
      otherClock: Map[NodeID, Int]): Boolean = {
    var earlier = true;
    for((node, clock) <- myClock
      if(node != from && otherClock(node) > myClock(node))) {
         earlier = false;
    }
     //If the above predicate was true for zero elements, it was earlier than
     //others
     earlier
  }

  def createOrdering(): CausalData = {
    val index = (me);
    clock = clock + 1;
    CausalData(vectorClock + (index -> clock ));
  }

  def setOrderCallback( callback: () => Int ) = { ; }
}

object Causal {
  def create(t : Communication,
             callbck : Message => Unit, thisNode: NodeID) : Causal = {
    val ord = new Causal(t, callbck,thisNode)
    t.setOnReceive(ord.receiveMessage)
    return ord
  }
}
