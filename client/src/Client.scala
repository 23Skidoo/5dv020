package gcom.client

import java.rmi.registry.LocateRegistry
import java.rmi.registry.Registry

import org.slf4j.LoggerFactory
import org.slf4j.Logger

import gcom._
import gcom.common.NodeID
import gcom.common.Util

import gcom.Communicator
import gcom.transport.Transport
import gcom.communication.Communication
import gcom.ordering.Ordering

object Client {

  /* Option parsing. */
  import org.clapper.argot.ArgotParser
  import org.clapper.argot.ArgotConverters._
  import org.clapper.argot.ArgotUsageException

  val parser = new ArgotParser(
    "gcom-client",
    preUsage=Some("gcom-client 0.1")
  )

  val portopt = parser.option[Int](List("p", "port"), "n", "port number")

  val reliablopt = parser.option[MulticastType](
    List("c", "communication"),
    "[reliable|unreliable]", "communication type") { (s, opt) =>
    s match {
      case "reliable" => ReliableMulticast()
      case "unreliable" => UnreliableMulticast()
      case _ => parser.usage("bad value for the -c option")
    }
  }

  val ordopt = parser.option[OrderingType](
    List("o", "ordering"),
    "[unordered|fifo|causal|total|causaltotal]", "ordering type") { (s, opt) =>
    s match {
      case "unordered" => NoOrdering()
      case "fifo" => FIFOOrdering()
      case "causal" => CausalOrdering()
      case "total" => TotalOrdering()
      case "causaltotal" => CausalTotalOrdering()
      case _ => parser.usage("bad value for the -o option")
    }
  }

  val nameserveropt = parser.option[NodeID](
    List("n", "nameserver"),
    "name:host:port", "node ID of the name server") { (s, opt) =>
    NodeID.fromString(s)
  }
  
  val lockingopt = parser.option[Int](
    List("l", "locking"),
    "n", "lock group to n nodes in group") { (s, opt) =>
    s.toInt  
  }
  

  sealed abstract class CommandType;
  case class CommandList() extends CommandType;
  case class CommandJoin(groupName : String) extends CommandType;
  case class CommandKill(groupName : String) extends CommandType;

  val commandopt = parser.multiParameter[String](
    "command", "[list|kill|join] [GROUP]", true)

  /* Values of the parsed options. */
  /* gcom-client -p PORT -c RELIABILITY -o ORDERING NAMESERVER COMMAND */
  lazy val port = portopt.value.getOrElse(31337)
  lazy val reliability = reliablopt.value.getOrElse(UnreliableMulticast())
  lazy val ordering = ordopt.value.getOrElse(NoOrdering())
  lazy val nameserver = nameserveropt.value.getOrElse(
    NodeID.fromString("nameserver:localhost:31337"))
  lazy val maxNodes = lockingopt.value.getOrElse(0);
  lazy val command = commandopt.value match {
    case "list" :: Nil => CommandList()
    case "kill" :: groupName :: Nil => CommandKill(groupName)
    case "join" :: groupName :: Nil => CommandJoin(groupName)
    case _ => parser.usage("no command or incorrect command")
  }
  val name   = Util.getRandomUUID
  val host   = Util.getLocalHostName
  val nodeID = new NodeID(name, host, port)
  val logger = LoggerFactory.getLogger(name)

  // Assemble a Communicator to be passed to the GUI.
  def assembleCommunicator(nsrv : NameServer,
                           group : Group)
      : (Communicator, Transport, Communication, Ordering) = {
    val transport =
      gcom.transport.BasicTransport.create(nodeID, {msg =>}, logger)
    val comm =
      reliability match {
        case UnreliableMulticast() =>
          gcom.communication.NonReliable.create(transport, {msg =>});
        case ReliableMulticast() =>
          gcom.communication.Reliable.create(transport, {msg =>},
                                             {() => Set[NodeID]()});
      }
    val ord =
        ordering match {
          case NoOrdering() =>
            gcom.ordering.NonOrdered.create(comm, {msg =>})
          case FIFOOrdering() =>
            gcom.ordering.FIFO.create(comm, {msg =>})
          case CausalOrdering() =>
            gcom.ordering.Causal.create(comm, {msg =>}, nodeID)
          case TotalOrdering() =>
            gcom.ordering.Total.create(comm, {msg =>}, {() => 0})
          case CausalTotalOrdering() =>
            gcom.ordering.CausalTotal.create(comm, {msg =>}, nodeID, {() => 0})
        }

    // Sets all the callbacks we've initialised with dummies above.
    val communicator =
      gcom.group.BasicGroup.create(group, logger, nsrv, nodeID,
                                   comm, ord, {msg =>})

    return (communicator, transport, comm, ord)
  }

  def main(args: Array[String]) = {
    try {
      parser.parse(args)
      logger.debug("Starting client")
      logger.debug("Registry on port: " + port.toString)
      logger.debug("Multicast type: " + reliability.toString)
      logger.debug("Ordering type: " + ordering.toString)
      logger.debug("Nameserver: " + nameserver.toString)
      logger.debug("Command: " + command.toString)
      val nsrv = connectToNameserver()
      command match {
        case CommandList()          => commandList(nsrv)
        case CommandKill(groupName) =>
          commandKill(nsrv, Group(groupName, reliability, ordering, maxNodes))
        case CommandJoin(groupName) =>
          commandJoin(nsrv, Group(groupName, reliability, ordering, maxNodes))
      }
    }
    catch {
      case e: ArgotUsageException => println(e.message)
    }
  }

  def connectToNameserver() : NameServer = {
    if (System.getSecurityManager == null) {
      System.setSecurityManager(new SecurityManager);
    }
    val registry = LocateRegistry.getRegistry(nameserver.host, nameserver.port)
    val stub = registry.lookup(nameserver.name).asInstanceOf[NameServer]

    logger.debug("Connected to the nameserver: " + nameserver.toString)
    return stub
  }

  def commandList(nsrv: NameServer) : Unit = {
    val groups = nsrv.listGroups()
    println("Groups:")
    if (groups.isEmpty) {
      println("none")
    }
    else{
      groups.foreach { group => println(group.toString) }
    }
  }

  def commandJoin(nsrv: NameServer, group : Group) = {
    try{
      val (communicator, transport, comm, ordering) =
            assembleCommunicator(nsrv, group)
      
      // Calls communicator's setOnReceive
      val debugGui =
      new gcom.client.gui.DebugGui(transport,
                                    ordering,
                                    communicator, comm)
      debugGui.visible = true
      val transportThread = new Thread(transport)
      transportThread.start()
          
    } catch {
      case e : Exception => {
        logger.error("Could not join group, " + e.getMessage())
        //Exit with general error
        System.exit(1);
      }
    }
  }

  def commandKill(nsrv: NameServer, group : Group) = {
    nsrv.getGroupLeader(group) match {
      case None => {
        println("Group " + group.name + " does not exist!")
      }
      case Some(leader) => {
        val tuple = assembleCommunicator(nsrv, group)
        logger.debug("Powering up the killing machine...")
        tuple._1.killGroup()
        System.exit(0)
      }
    }
  }
}
