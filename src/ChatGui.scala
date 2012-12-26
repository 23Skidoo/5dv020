
import swing._
import swing.event._
import java.awt.FlowLayout

class DummyCommunicator(callBack: String => Unit){
  def broadCastMsg(msg: String){
      callBack(msg);
  }
  def leaveGroup() = {println( "Leaving" )}
}

object DummyNameServer{
  def joinGroup(g: JoinGroup, onRecv: String => Unit) = {
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
    List(Group("Group1", Reliable(),FIFO()), Group("Group2", NonReliable(),Total()));
  }
}

object SwingApp extends SimpleSwingApplication {
  
  //initialize NameServer
 
  
  def top = new MainFrame {
    title = "ChatGui"
      
    import Dialog._
    val possibilities = DummyNameServer.listGroups;
    val buttons = new BoxPanel(Orientation.Vertical) {
        
      }
    val s = showInput[Group](buttons,
      "Select a server",
      "Server Selection",
      Message.Plain, 
      Swing.EmptyIcon,
      possibilities, null)
   
    if(s.isEmpty){
      quit()
    }  
      
    val com = DummyNameServer.joinGroup(
        ExistingGroup(s.get.groupName), 
        { msg => 
          chatBox.append("\n" + msg)
          scroller.verticalScrollBar.value = scroller.verticalScrollBar.maximum;
         });
    object button extends Button {
      text = "Send"
    }
    object chatBox extends TextArea{
      text = "Welcome to chat"
      editable = false;
      listenTo(button)
      reactions += {
        case ButtonClicked(button) =>
          com.broadCastMsg(chatInput.text)
          //append(chatInput.text);
      }
      
    }
    object scroller extends ScrollPane{
      contents = chatBox;
    }
    object chatInput extends TextField{
     /* minimumSize = new Dimension(200,50)
      maximumSize = new Dimension(300,50)*/
      preferredSize = new Dimension(250,30)
      horizontalAlignment = Alignment.Left;
    };
    
    object inputPanel extends FlowPanel{
      contents.append(chatInput, button);
    }
    
    contents = new BoxPanel(Orientation.Vertical) {
      contents.append(scroller, inputPanel)
      border = Swing.EmptyBorder(5, 5, 5, 5)
    }
      
    override def closeOperation(){
      com.leaveGroup
      super.closeOperation
    }
  }

}

