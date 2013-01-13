package gcom.client.gui

import swing._
import swing.event._
import java.awt.FlowLayout
import javax.swing.{UIManager}
import java.awt.GridBagConstraints
import java.awt.GridBagConstraints
import gcom.Group
import gcom.common._
import gcom.transport._
import gcom.Communicator
import gcom.ordering.Ordering
import gcom.communication.Communication
import gcom.ordering.NonOrdered
import scala.swing.ListView.Renderer

/**
 * Provides 
 * top                    : Returns a mainframe with debugging GUI
 * showGroupSelectDialog  : Shows a dialog where the user can select a group
 */
class DebugGui(
    t: Transport,
    o: Ordering, 
    communicator: Communicator,
    com: Communication) extends MainFrame {

  //initialize NameServer
  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)
  t match {
    case x: BasicTransport => listenTo(x) 
  }
  listenTo(o)
  communicator.setOnReceive(sendFunction(_))
  
  var messageQueues = Map[Ordering,(String,List[String])]();
  var messageCounter = 0
  
  def showGroupSelectDialog(possibilities: List[Group]): Option[Group] = {
    import Dialog._
    val buttons = new BoxPanel(Orientation.Vertical)
    val groupToJoin = showInput[Group](buttons,
      "Select a server",
      "Server Selection",
      scala.swing.Dialog.Message.Plain,
      Swing.EmptyIcon,
      possibilities, null)
    groupToJoin
  }
  
  
  object chatBox extends TextArea{
    text = "Welcome to chat"
    editable = false;
  }

  object scrollingChatBox extends ScrollPane{
    contents = chatBox;
  }

  def sendFunction(msg: String) {
        chatBox.append("\n" + msg)
          /* Work around bug where maximum was not yet updated, let swing
           * handle scrolling down.
           */
          javax.swing.SwingUtilities.invokeLater(new Runnable() {
                  def run() {
                      val bar = scrollingChatBox.verticalScrollBar
                      bar.value = bar.maximum
                  }
              }
         );
  }

  
    title = "ChatGui"

    object dropText extends Label("Delay: ")
    object dropInput extends CheckBox
    object delayText extends Label("Drop: ")
    object delayInput extends TextField{
      columns = 3;
    }

    object queueList extends ListView[(String,Ordering)]{
      listData = List()
      renderer = Renderer(_._1)
    }

    object messageList extends ListView[String]{
      listData = List("Messages are to be displayed here")
    }


    object button extends Button {
      text = "Send"
    }

    object nodeList extends ListView[String]{
      listData = communicator.listGroupMembers.map(_.toString).toList
    }

    object counter extends Label("0 Messages")


    object chatInput extends TextField{
      preferredSize = new Dimension(250,30)
      horizontalAlignment = Alignment.Left;
    };

    object inputPanel extends FlowPanel{
      //For some reason objects are not displayed in the same order as added
      contents.append(counter,
          chatInput,
          delayText,
          dropInput,
          dropText,
          delayInput,
          button);
    }

    contents = new GridBagPanel {
      import java.awt.GridBagConstraints._;
      val chatAndGroupMembers = new GridBagPanel {
        add(scrollingChatBox, new Constraints(){
          grid = (0,0)
          gridheight = 1
          gridwidth = 1
          weightx = 1
          weighty = 1
          anchor = GridBagPanel.Anchor.LineStart
          fill = GridBagPanel.Fill.Both
        })
        add(new ScrollPane(nodeList), new Constraints(){
          grid = (1,0)
          gridheight = 1
          gridwidth = 1
          weightx = 0.2
          weighty = 1
          anchor = GridBagPanel.Anchor.LineStart
          fill = GridBagPanel.Fill.Both

        })
      }
      //contents.append(scroller, inputPanel)
      addComponent(new ScrollPane(queueList),  0, 0, 1, 1, 1 ,1, NORTH,  BOTH)
      addComponent(new ScrollPane(messageList),1, 0, 1, 1, 1 ,1, NORTH,  BOTH)
      addComponent(chatAndGroupMembers,        0, 1, 2, 1, 1 , 1, NORTH, BOTH)
      addComponent(inputPanel,                 0, 2, 2, 1, 1 , 0, NORTH, BOTH)

      border = Swing.EmptyBorder(5, 5, 5, 5)

      def addComponent(component: Component, x: Int, y: Int,
                       width: Int, height: Int,
                       weightx: Double, weighty: Double,
                       anchor: Int, fill: Int): Unit = {
      val gbc = new GridBagConstraints
      gbc.gridx = x
      gbc.gridy = y
      gbc.gridwidth = width
      gbc.gridheight = height
      gbc.weightx = weightx
      gbc.weighty = weighty
      gbc.anchor = anchor
      gbc.fill = fill
      add(component,new Constraints(gbc))
    }
 
    }


    listenTo(queueList.selection)
    reactions += {
      case UpdateQueue(key, header, list) => {
        val selection = queueList.selection.anchorIndex;
        messageQueues += (key -> (header, list))
        updateList();
        queueList.selectIndices(selection)

      }
      // `` lets us match against that specific variable.
      case ListSelectionChanged(`queueList`,range,live) => {
        val key = queueList.listData(queueList.selection.anchorIndex)
        messageList.listData = messageQueues.getOrElse(key._2, ( "", List() ) )._2
      }
      case UpdateSentMessages(num) => {
        messageCounter += 1
        counter.text = messageCounter.toString + " Messages"
      }
    }
    listenTo(button)
    listenTo(chatInput)
    reactions += {
      case ButtonClicked(button) => sendMsg()
    }
    def sendMsg() = {
       val drop = dropInput.selected;
       val delay = try{
          delayInput.text.toInt
       } catch{
         case e: NumberFormatException =>
          0
       }
       messageCounter = 0
       com.setDelay(delay);
       com.setDrop(drop)
       communicator.broadcastMessage(chatInput.text)
    }


    private def updateList() = {
      queueList.listData = messageQueues.map({
        case (ordering, (header,list)) => (header, ordering)
      }).toList
    }

    override def closeOperation(){
      communicator.leaveGroup
      super.closeOperation
    }
  

}
