import scala.swing.SimpleSwingApplication
import java.rmi.server.UnicastRemoteObject
import java.rmi.registry.LocateRegistry
import scala.swing.ListView
import scala.swing.TextArea
import scala.swing.MainFrame
import scala.swing.Component
import scala.swing.GridPanel
import scala.swing.ScrollPane
import scala.swing.Swing
import scala.swing.Publisher
import java.awt.Dimension

object NameServerGui extends SimpleSwingApplication {
  
  //Get these from config
  
  val NSNAME = "ns";
  
  //Warning! Using null
  var registry = LocateRegistry.createRegistry(Config.PORT);
  var ns = new PublishingNameServer();
  val stub = UnicastRemoteObject.exportObject(ns, 0);
  registry.bind(NSNAME, stub);
  
  val groupList = new ListView[Group]{
    listData = List();
  }
  
  val memberList = new ListView[Node]{
    listData = List();
  }
  val logPane = new TextArea(){
    text = "Starting NameServer\n"
    editable = false;
  }
  def top = new MainFrame{
    title = "NameServer" 
    preferredSize = new Dimension(600,400);
    listenTo(ns);
    contents = new GridPanel(2,1){
      contents.append(createGroupPanel(), new ScrollPane(logPane)) 
    }
     
  }
  
  def createGroupPanel(): Component = {
    new GridPanel(1,2){
        contents.append(
            new ScrollPane{ contents = groupList },
            new ScrollPane{ contents = memberList })
        border = Swing.EmptyBorder(5);
      }
  }
  
  listenTo(groupList.selection)
  reactions += {
    /*case ListSelectionChanged(list, range, true) =>
      val groupList = list.asInstanceOf[ListView[Group]]
      //memberList.listData = groupList.map( ns.getDistinguished(_).getMe )*/
    case GroupUpdateEvent(groups) => groupList.listData = groups;
  }
}

case class GroupUpdateEvent(groups: List[Group]) extends scala.swing.event.Event; 

class PublishingNameServer extends NameServerImplementation with Publisher{
  override def insertGroup(group: Group) = {
    super.insertGroup(group);
    publish(GroupUpdateEvent(listGroups))
  }
}
