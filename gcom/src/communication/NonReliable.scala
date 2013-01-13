package gcom.communication;

import gcom.common._;
import gcom.transport.Transport;

class NonReliable(t : Transport, callbck : Message => Unit)
                 extends Communication(t, callbck) {
  
  def sendToAll(dsts : Set[NodeID],
                payload: String, ordering: OrderingData) : List[NodeID] = {
    val msg = Message(NoReliabilityData(), ordering, payload)
    sendWithDelay(dsts, msg)
  }

  def receiveMessage(msg : Message) = callback(msg)

  def setHostCallback (callbck : () => Set[NodeID]) = { ; }
}

object NonReliable {
  def create(t : Transport, callbck : Message => Unit) : NonReliable = {
    val comm = new NonReliable(t, callbck)
    t.setOnReceive(comm.receiveMessage)
    return comm
  }
}
