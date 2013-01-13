package gcom.ordering;

import gcom.common.Message
import gcom.communication.Communication
import gcom.common.NodeID
import gcom.common.OrderingData
import scala.swing.Publisher

abstract class Ordering (
    c: Communication, 
    callbck : Message => Unit) 
  extends Publisher {
  
  var callback = callbck
  val communicator = c;
  def setOnReceive(callbck : Message => Unit) = callback = callbck
  def receiveMessage(msg : Message) : Unit
  def sendToAll(dst: List[NodeID],payload: String) : Unit = {
    communicator.sendToAll(dst, payload,createOrdering())
  }
  protected def createOrdering() : OrderingData
}
