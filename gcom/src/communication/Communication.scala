package gcom.communication;

import gcom.common._
import gcom.transport.Transport;
import scala.compat.Platform

abstract class Communication(t : Transport, callbck : Message => Unit) {
  val transport = t
  var callback  = callbck

  var delay: Long = 0
  var drop = false

  /** Send a message to all nodes in the list. Return the IDs of those that were
   *  reachable. */
  def sendToAll(dst : Set[NodeID],
                payload : String, ordering: OrderingData) : Set[NodeID]

  def setDelay(delay: Long) = this.delay = delay
  def setDrop(drop: Boolean) = this.drop = drop

  protected def sendWithDelay(dsts: Set[NodeID], msg: Message): Set[NodeID] = {

    if(delay == 0) {
      dsts.foreach ({ dst =>
        transport.sendMessage(dst, msg)
      })
    }
    else{
      new Thread({new Runnable(){
        def run() {
          Thread.sleep(delay*1000)
          dsts.foreach ({ dst =>
            transport.sendMessage(dst, msg)
          })
        }
      }}).start()
      
    }
    dsts.filter({dst => transport.pingNode(dst)})
  }
  /** Change the onReceive callback. */
  def setOnReceive(callbck : Message => Unit) : Unit = callback = callbck

  /** For the Reliable method. */
  def setHostCallback (callbck : () => Set[NodeID])
}
