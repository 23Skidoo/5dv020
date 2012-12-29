import java.rmi.registry.LocateRegistry
import java.rmi.registry.Registry
import java.rmi.server.UnicastRemoteObject
import java.rmi.Remote
import java.rmi.RemoteException
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import scala.swing.ListView

/**
 * The remote interface the NameServer object communicates through 
 */
trait NameServerTrait extends Remote{
  @throws(classOf[RemoteException])
  def listGroups: Seq[Group]
  @throws(classOf[RemoteException])
  def createId: Int
  @throws(classOf[RemoteException])
  def getGroupInfo(groupName: String): Option[Group]
  @throws(classOf[RemoteException])
  def updateDistinguished(group: Group, node: Node)
  @throws(classOf[RemoteException])
  def getDistinguished(group: String): Node
  @throws(classOf[RemoteException])
  def getDistinguished(group: Group): Node
  @throws(classOf[RemoteException])
  def insertGroup(group: Group)
}

/**
 * An implementation of the NameServerTrait. A NameServer application exposes this 
 * object via RMI so other clients using the NameServer object can connect to it. NameServerGui.scala
 * contains an NameServer Application
 */
class NameServerImplementation extends NameServerTrait{
  protected var groups = Map[String,Group]();
  protected var distinguished = Map[Group,Node]();
  
  def listGroups = List(groups.values.toSeq: _*)
  
  def getGroupInfo(groupName: String) = groups.get(groupName)
  
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
  def createJoinMessage(from: Node): Message = Message(NonReliableMessage(from), SystemMessage(JOIN()))
}

/**
 * This object is exposed to users of this middleware so they can join and leave groups.
 * 
 */
object NameServer {
  implicit def GroupToString(g: ExistingGroup): String = g.groupName
  
  val ns = LocateRegistry.getRegistry(Config.HOST,Config.PORT)
    .lookup(Config.NSNAME).asInstanceOf[NameServerTrait]
  
  private def buildCommunicator[T](group: Group, onRecv: T => Unit): Communicator[T] = {
    val id = ns.createId;
    new Communicator(id,onRecv,group);
  }
  
  def joinGroup[T](group: JoinGroup, onRecv: (T => Unit)): Communicator[T] = { group match {
    case ExistingGroup(name) => {
      ns.getGroupInfo(name) match {
        case Some(group: Group) => {
          val joiner = buildCommunicator[T](group, onRecv)
          ns.getDistinguished(group).ref.recv(MessageFactory.createJoinMessage(joiner.me));
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
  //TODO Kill groups
  //killGroup(Group g) : Bool
  def listGroups() : Seq[Group] = ns.listGroups
}