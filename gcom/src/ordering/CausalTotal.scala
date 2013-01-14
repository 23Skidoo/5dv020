package gcom.ordering;

import gcom.communication.Communication
import gcom.common.Message
import gcom.common.NodeID
import gcom.common.OrderingData
import gcom.common.CausalTotalData
import gcom.common.UpdateQueue
import gcom.common.UpdateSentMessages
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

  //Forward messages from total and causal
  listenTo(total)
  listenTo(causal)
  deafTo(this)
  reactions += {
    case x: UpdateSentMessages => publish(x)
  }
  reactions += {
    case x: UpdateQueue => publish(x)
  }

  def receiveMessage(msg : Message) = { msg match {
    case Message(_, data : CausalTotalData, _) => {
      if(!total.initialized){ 
        total.initialize(data.order)
      }
      causal.receiveMessage(msg)
    } 
    case msg => callback(msg)
  }
      
  }

  override def sendToAll(dsts: Set[NodeID],payload: String) : Set[NodeID] = {
    communication.sendToAll(dsts, payload, createOrdering())
  }
  override protected def createOrdering() : OrderingData = {
    val totalData = total.createOrdering
    val causalData = causal.createOrdering
    CausalTotalData(causalData.clock, totalData.order)
  }

  def setOrderCallback( callback: () => Int ) = {
    total.setOrderCallback(callback);
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
