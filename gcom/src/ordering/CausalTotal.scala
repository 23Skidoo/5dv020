package gcom.ordering;

import gcom.communication.Communication
import gcom.common.Message
import gcom.common.NodeID
import gcom.common.OrderingData
import gcom.common.CausalTotalData

class CausalTotal(
    c: Communication,
    callbck : Message => Unit,
    thisNode: NodeID,
    nextOrder : () => Int)
  extends 
    Ordering (c: Communication, callbck : Message => Unit) {
  
  val total =  Total.create(c, callback, nextOrder)
  val causal = Causal.create(c, {println("causal deliver"); total.receiveMessage(_) }, thisNode)
  
  def receiveMessage(msg : Message) = {
	  causal.receiveMessage(msg)
  }

  def updateView(newClock: Map[NodeID, Int], order: Int){
    causal.updateView(newClock)
    total.updateView(order)
  }
  
  override def sendToAll(dst: List[NodeID],payload: String) : Unit = {
    communicator.sendToAll(dst, payload,createOrdering())
  }
  override protected def createOrdering() : OrderingData = {
    val totalData = total.createOrdering
    val causalData = causal.createOrdering
    CausalTotalData(causalData.clock, totalData.order)
  }
}

object CausalTotal {
  def create(
      t : Communication,
      callbck : Message => Unit,
      thisNode: NodeID,
      nextOrder: () => Int ) : CausalTotal = {
    val ord = new CausalTotal(t, callbck,thisNode, nextOrder)
    t.setOnReceive(ord.receiveMessage)
    return ord
  }
}
