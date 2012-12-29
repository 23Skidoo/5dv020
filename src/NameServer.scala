import java.rmi.registry.LocateRegistry
import java.rmi.registry.Registry
import java.rmi.server.UnicastRemoteObject
import java.rmi.Remote
import java.rmi.RemoteException
import scala.swing.Publisher
import java.io.FileInputStream
import java.util.Properties
import scala.swing.SimpleSwingApplication
import scala.swing.MainFrame
import java.awt.Dimension
import java.io.FileOutputStream
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import scala.swing.ListView
import java.awt.FlowLayout
import scala.swing.FlowPanel
import scala.swing.BoxPanel
import scala.swing.Orientation
import scala.swing.BorderPanel
import java.awt.BorderLayout
import javax.swing.JPanel
import scala.swing.ScrollPane
import scala.swing.GridPanel
import scala.swing.Swing
import scala.swing.Component
import javax.swing.text.html.HTMLEditorKit
import javax.swing.JEditorPane
import scala.swing.TextArea
import scala.swing.event.ListSelectionChanged


object Config {
  val logger = LoggerFactory.getLogger(this.getClass()); 
  val defaultProps = new Properties();
  //TODO Why is resources not available?
  val in = new FileInputStream("src/resources/config.properties");
  defaultProps.load(in);
	  
	  
  val PORT =   defaultProps.getProperty("port").toInt
  val HOST =   defaultProps.getProperty("host")
  val NSNAME = defaultProps.getProperty("nsname")
  in.close();
}

object NameServerGui extends SimpleSwingApplication {
  
  //Get these from config
  
  val NSNAME = "ns";
  
  //Warning! Using null
  var registry:Registry = LocateRegistry.createRegistry(Config.PORT);
  var ns: RemoteNameServer = new RemoteNameServer();
  val stub = UnicastRemoteObject.exportObject(ns, 0);
  registry.bind(NSNAME, stub);
  
  val groupList = new ListView[List[Node]]{
    listData = List();
  }
  
  val memberList = new ListView[String]{
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
    case ListSelectionChanged(list, range, true) =>
      val a = list.selection.items.head.asInstanceOf[List[Node]]
      memberList.listData = a map (_.toString)
  }
  
  //Move this to clients
  /*
  def init(host: String){
    	registry = LocateRegistry.getRegistry(host,PORT);
	    ns = registry.lookup(NSNAME).asInstanceOf[AuthorizedServer];
  }
  */
  def listGroups() : List[Group] = ns.listGroups

}

trait NameServerTrait extends Remote{
  @throws(classOf[RemoteException])
  def listGroups: Seq[Group]
}

class RemoteNameServer extends NameServerTrait with Publisher{
  var groups = Map[String,Group]();
  var distinguished = Map[Group,Node]();
  
  def listGroups = List(groups.values.toSeq: _*)
  
  def getGroupInfo(groupName: String) = {
    groups.get(groupName)
  }
  
  def createId(): Int = generateName
  
  def getDistinguished(group: Group): Node = distinguished(group)
  def getDistinguished(group: String): Node = distinguished(groups(group))
  
  def updateDistinguished(group: Group, node: Node){
    distinguished += group -> node;
  }
  
  var numNodes = 0;
  private def generateName(): Int = { numNodes += 1; numNodes }
  
  def insertGroup(group: Group) = groups += (group.groupName -> group)
  
}

object MessageFactory{
  def createJoinMessage() = {
  }
}

object NameServer {
  implicit def GroupToString(g: ExistingGroup): String = g.groupName
  
  val ns = LocateRegistry.getRegistry(Config.HOST,Config.PORT)
    .lookup(Config.NSNAME).asInstanceOf[RemoteNameServer]
  
  private def buildCommunicator[T](group: Group, onRecv: T => Unit): Communicator[T] = {
    //Should we pack these calls into one call?
    val id = ns.createId;
    val leaderNode = ns.getDistinguished(group.groupName)
    
    //TODO build GroupCom here
    
    //TODO these should be provided by groupCom
    val onErrorPlaceHolder = {p: List[Node] => } 
    val hostCallBackHolder = {() => List[Node]() } 
    val internal = group match {
      case Group(_,comType: NonReliable, _) => new BasicCom[T](id, onErrorPlaceHolder)
      case Group(_,comType: Reliable, _) => new ReliableCom[T](id, onErrorPlaceHolder, hostCallBackHolder)
      
    }
    val node = RefNode(internal,id);
    val order = group match {
      case Group(_,_,ordering: Unordered) => new UnorderedQueue[T]()
      case Group(_,_,ordering: FIFO)      => new FIFOQueue[T]()
      case Group(_,_,ordering: Causal)    => new CausalQueue[T](node)
      //How do we get the  callback to the totalorderqueue?
      //case Group(_,_,ordering: Total) =>     new TotalOrderQueue[T]()
    } 
    new Communicator(id,onRecv,internal,order);
  }
  
  def createJoinMessage(from: Node): Message = Message(NonReliableMessage(from), SystemMessage(JOIN()))
  
  def joinGroup[T](group: JoinGroup, onRecv: (T => Unit)): Communicator[T] = { group match {
    case ExistingGroup(name) => {
      ns.getGroupInfo(name) match {
        case Some(group: Group) => {
          val joiner = buildCommunicator[T](group, onRecv)
          ns.getDistinguished(group).ref.recv(createJoinMessage(joiner.me));
          joiner
        }
        case None => throw new Exception("No such group")
      }
      
    }
    case NewGroup(name, reliability, ordering) => {
      ns.getGroupInfo(name) match {
        case None => {
          val newGroup = new Group(name,reliability,ordering);
          val leader = buildCommunicator[T](newGroup, onRecv);
          ns.updateDistinguished(newGroup, leader.me)
          ns.insertGroup(newGroup);
          leader
        }
        case Some(x) => throw new Exception("Group Already Exists.")
      }  
    }
  }
    
  }
  //killGroup(Group g) : Bool
  def listGroups() : List[Group] = ns.listGroups
}