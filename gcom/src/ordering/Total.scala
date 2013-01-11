package gcom.ordering;

import gcom.common.Message
import gcom.communication.Communication
import gcom.transport.Transport
import gcom.common.TotalOrdering


/**
 * callbck is executed on recieved messages when they are to be delivered
 * nextOrder is run on each createMessage to order the message
 */
class Total(c: Communication, callbck : Message => Unit, nextOrder : () => Int) extends Ordering(c, callbck) {
  var order = 0;
  var holdBack = List[(Message, TotalOrdering)]();
  private var getNextOrder = nextOrder;
  def receiveMessage(msg : Message) { msg match{
    case Message(_,totalMsg: TotalOrdering,_) => {
      /*
       * We want to get every consecutive message from holdbacks
       * 
       * By sorting and zipping each message with its number we can 
       * then group the holdback by the messages that are in order 
      */
      val zippedWithIndex = ((msg,totalMsg) :: holdBack).sortBy(_._2.order) zipWithIndex
      //GroupBy returns a map Boolean -> Result
      val consecMap = shiftIndex(zippedWithIndex, order).groupBy(t => t match {
        case ((m,msgOrder), mOrder) => msgOrder.order == mOrder;
      })
      
      holdBack = consecMap.get(false).getOrElse(List()).map(_._1)
      consecMap.get(true).getOrElse(List()).foreach({ case ((msg,_),_) =>
        callbck(msg)
        order = order + 1
      })
      
      
    }
    case msg: Message => callback(msg)
    } 
  }
  
  private def shiftIndex(list: List[((Message, TotalOrdering), Int)], amount: Int) = {
    list.map(a => (a._1,a._2+amount))
  }

  def updateView(order: Int) = this.order = order
  def createOrdering() = TotalOrdering( getNextOrder() )
  def setOrderCallback( callback: () => Int ) = getNextOrder = callback;
}

object Total {
  def create(t : Communication, callbck : Message => Unit, nextOrder : () => Int ) : Total = {
    val ord = new Total(t, callbck, nextOrder)
    t.setOnReceive(ord.receiveMessage)
    return ord
  }
}
