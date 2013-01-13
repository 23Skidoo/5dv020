
import swing._
import swing.event._
import java.awt.FlowLayout
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
    List(Group("Group1", ReliableMulticast(),FIFOOrdering()),
         Group("Group2", UnreliableMulticast(),TotalOrdering()));
  }
}

object ChatGui extends SimpleSwingApplication {

  //initialize NameServer
  //Client.parser.parse(args)

  def top = new MainFrame {
    title = "ChatGui"

    import Dialog._
    val possibilities = DummyNameServer.listGroups;
    val buttons = new BoxPanel(Orientation.Vertical) {

      }
    val s = showInput[Group](buttons,
      "Select a server",
      "Server Selection",
      Message.Question, 
      Swing.EmptyIcon,
      possibilities, null)

    if(s.isEmpty){
      quit()
    }

    val com = DummyNameServer.joinGroup(
        s.get, 
        { msg => 
          chatBox.append("\n" + msg)
          javax.swing.SwingUtilities.invokeLater(new Runnable() {
                  def run() {
                      val bar = chatScroller.verticalScrollBar
                      bar.value = bar.maximum
                  }
              }
          );
         });
    object button extends Button {
      text = "Send"
    }
    object chatBox extends TextArea{
      text = "Welcome to chat"
      editable = false;

    }
    object chatScroller extends ScrollPane{
      contents = chatBox;
    }
    object chatInput extends TextField(30){
      horizontalAlignment = Alignment.Left;
    };
    
    object nodeList extends ListView(List("Waiting for group info"))
    
    object inputPanel extends FlowPanel{
      contents.append(chatInput, button);
    }
    
    contents = new BoxPanel(Orientation.Vertical) {
      contents.append(new BoxPanel(Orientation.Horizontal){
       contents.append(chatScroller, new ScrollPane(nodeList)) 
      }, inputPanel)
      border = Swing.EmptyBorder(5, 5, 5, 5)
    }
    
    listenTo(button)
    listenTo(chatInput)
    reactions += {
      case ButtonClicked(button) =>
        com.broadcastMessage(chatInput.text)
      case EditDone(_) =>
        com.broadcastMessage(chatInput.text)
    }
      
    override def closeOperation(){
      com.leaveGroup
      super.closeOperation
    }
  }

}

