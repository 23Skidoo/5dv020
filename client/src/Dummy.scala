package gcom.client.gui

import gcom.Group
import gcom.Communicator
import gcom.FIFOOrdering
import gcom.ReliableMulticast
import gcom.TotalOrdering
import gcom.UnreliableMulticast
import gcom.client._

class DummyCommunicator(callBack: String => Unit) {
  def broadcastMessage(msg: String){
      callBack(msg);
  }
  def leaveGroup() = {println( "Leaving" )}

}

object DummyNameServer {
  def joinGroup(g: Group, onRecv: String => Unit) = {
    new Thread(new Runnable{
      def run(){
        for(i <- 1 to 100){
          onRecv("Hello " + i)
          Thread.sleep(100);
        }
      }
    }).start();

    new DummyCommunicator(onRecv);
  }
  def listGroups(): List[Group] = {
    List(Group("Group1", ReliableMulticast(),FIFOOrdering(), 0),
         Group("Group2", UnreliableMulticast(),TotalOrdering(), 0));
  }
}
