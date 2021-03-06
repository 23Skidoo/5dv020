package gcom.ordering;

import gcom.common.Message
import gcom.communication.Communication
import gcom.transport.Transport
import gcom.common.TotalOrdData
import gcom.common.IsTotal
import gcom.common.UpdateQueue


/**
 * callbck is executed on recieved messages when they are to be delivered
 * nextOrder is run on each createMessage to order the message
 */
class Total(c: Communication,
            callbck : Message => Unit, nextOrder : () => Int)
      extends Ordering(c, callbck) {
  var order = 0;
  var initialized = false
  var holdBacks = List[(Message, IsTotal)]();
  private var getNextOrder = nextOrder;
  
  def receiveMessage(msg : Message) { msg match{
    case Message(_,totalMsg: IsTotal,_) => {
      /*
       * We want to get every consecutive message from holdbacks
       *
       * By sorting and zipping each message with its number we can
       * then group the holdback by the messages that are in order
      */
      if(!initialized) {
        order = totalMsg.order
        initialized = true
      }
      if(totalMsg.order < order){
        return
      }
      val zippedWithIndex =
        ((msg,totalMsg) :: holdBacks).sortBy(_._2.order) zipWithIndex
      //GroupBy returns a map Boolean -> Result
      val consecMap = shiftIndex(zippedWithIndex, order).groupBy(t => t match {
        case ((m,msgOrder), mOrder) => msgOrder.order == mOrder;
      })
      holdBacks = consecMap.get(false).getOrElse(List()).map(_._1)
      consecMap.get(true).getOrElse(List()).foreach({ case ((msg,_),_) =>
        callback(msg)
        order = order + 1
      })
      publish(UpdateQueue(this,
          "Total: " + order,
          consecMap.getOrElse(false, List()) map (p => "" + p._1._1 + " " + p._2) ))

    }
    case msg: Message => callback(msg)
    }
  }

  private def shiftIndex(list: List[((Message, IsTotal), Int)],
                         amount: Int) = {
    list.map(a => (a._1,a._2+amount))
  }

  def createOrdering() = {
    val next = getNextOrder()
    if(! initialized){
      order = next
      initialized = true
    }
    TotalOrdData(next)
  }
  
  def initialize(order: Int){
    this.order = order
    initialized = true
  }
  
  def setOrderCallback( callback: () => Int ) = getNextOrder = callback;
}

object Total {
  def create(t : Communication,
             callbck : Message => Unit, nextOrder : () => Int ) : Total = {
    val ord = new Total(t, callbck, nextOrder)
    t.setOnReceive(ord.receiveMessage)
    return ord
  }

}
