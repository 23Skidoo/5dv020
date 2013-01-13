package gcom.communication;

import gcom.common._
import gcom.transport.Transport;
import scala.compat.Platform

abstract class Communication(t : Transport, callbck : Message => Unit) {
  val transport = t
  var callback  = callbck

  var delay: Long = 0
  var drop = false
  var delayedMessages = List[(Message,Long)]()
  
  /** Send a message to all nodes in the list. Return the IDs of those that were
   *  reachable. */
  def sendToAll(dst : Set[NodeID],
                payload : String, ordering: OrderingData) : List[NodeID]

  def setDelay(delay: Long) = this.delay = delay
  def setDrop(drop: Boolean) = this.drop = drop
  
  protected def sendWithDelay(dsts: Set[NodeID], msg: Message): List[NodeID] = {
    var retList = List[NodeID]()
    val mightSend = delayedMessages.groupBy({
      case (msg, delayedTime) => {
          delayedTime < Platform.currentTime
      }
    })
    delayedMessages = mightSend.getOrElse(false, List())
    mightSend.getOrElse(true, List()).map({ case (msg, _) =>
      dsts.foreach({
        transport.sendMessage(_, msg)
      })
      
    })
    if(delay == 0) {
      dsts.foreach ({ dst =>
        val mid = transport.sendMessage(dst, msg)
        retList = mid.map({ id => id :: id :: retList}).getOrElse(retList)
      })
      retList
    }
    else{
      delayedMessages = 
        (msg, Platform.currentTime+delay*1000) :: (msg, Platform.currentTime+delay*1000) :: delayedMessages
      dsts toList
    }
    
  }
  /** Change the onReceive callback. */
  def setOnReceive(callbck : Message => Unit) : Unit = callback = callbck

  /** For the Reliable method. */
  def setHostCallback (callbck : () => Set[NodeID])
}
