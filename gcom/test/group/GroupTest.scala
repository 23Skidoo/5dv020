package gcom.test

import java.rmi.registry.LocateRegistry

import org.scalatest._
import org.slf4j.LoggerFactory
import gcom._
import gcom.common._
import gcom.transport._
import gcom.communication._
import gcom.ordering._
import gcom.group._


/** Requires an rmiregistry and nameserver running on port 31337. */
class GroupSpec extends FlatSpec with BeforeAndAfter {
  behavior of "the group management module"

  val host       = Util.getLocalHostName
  val port       = 31337
  val nameserver = connectToNameserver(NodeID("nameserver", host, port))
  val logger     = LoggerFactory.getLogger("groupspec")
  val group      = Group("default", UnreliableMulticast(), NoOrdering(), 0)

  // Dummies, not used
  val transport     = BasicTransport.create(NodeID("dummy", host, port),
                                            {msg => }, logger);
  val communication = NonReliable.create(transport, {msg =>})
  val ordering      = NonOrdered.create(communication, {msg => })

  // Helpers
  def connectToNameserver(nameserver : NodeID) : NameServer = {
    val registry = LocateRegistry.getRegistry(nameserver.host, nameserver.port)
    val stub = registry.lookup(nameserver.name).asInstanceOf[NameServer]

    return stub
  }

  before {
    nameserver.clearGroups()
  }

  after {
    nameserver.clearGroups()
  }

  "Empty name server" should "be running" in {
    val grps = nameserver.listGroups()
    assert(grps.isEmpty)
  }

  "Single nodes" should "be able to create singleton groups" in {
    val nodeID = new NodeID("node1", host, port)
    val communicator =
      BasicGroup.create(group, logger, nameserver,
                        nodeID, communication, ordering, {msg =>})

    assert(communicator.state.leader === nodeID)
    assert(communicator.state.members.size === 1)
  }

  "Nodes" should "be able to join groups" in {
    val node1 = new NodeID("no1", host, port)
    val node2 = new NodeID("no2", host, port)
    val communicator1 =
      BasicGroup.create(group, logger, nameserver,
                        node1, communication, ordering, {msg =>})
    val communicator2 =
      BasicGroup.create(group, logger, nameserver,
                        node2, communication, ordering, {msg =>})

    assert(communicator1.state.leader === node1)
    assert(communicator2.state.leader === node1)
    assert(communicator1.state.members.size === 2)
    assert(communicator2.state.members.size === 2)
    assert(communicator1.state.members === communicator2.state.members)
    assert(communicator1.state.members === Map(node1 -> 0, node2 -> 1))

    val node3 = new NodeID("node3", host, port)
    val communicator3 =
      BasicGroup.create(group, logger, nameserver,
                        node3, communication, ordering, {msg =>})

    assert(communicator3.state.leader === node1)
    assert(communicator1.state.members.size === 3)
    assert(communicator2.state.members.size === 3)
    assert(communicator3.state.members.size === 3)
    assert(communicator1.state.members === communicator2.state.members)
    assert(communicator3.state.members === Map(node1 -> 0, node2 -> 1,
                                               node3 -> 2))
  }

  "Nodes" should "be able to leave groups" in {
    val node1 = new NodeID("node1", host, port)
    val node2 = new NodeID("node2", host, port)
    val node3 = new NodeID("node3", host, port)
    val communicator1 =
      BasicGroup.create(group, logger, nameserver,
                        node1, communication, ordering, {msg =>})
    val communicator2 =
      BasicGroup.create(group, logger, nameserver,
                        node2, communication, ordering, {msg =>})
    val communicator3 =
      BasicGroup.create(group, logger, nameserver,
                        node3, communication, ordering, {msg =>})

    assert(communicator1.state.members === communicator2.state.members)
    assert(communicator3.state.members === Map(node1 -> 0, node2 -> 1,
                                               node3 -> 2))

    communicator3.leaveGroup()
    assert(communicator1.state.members === communicator2.state.members)
    assert(communicator2.state.members === Map(node1 -> 0, node2 -> 1))

    communicator2.leaveGroup()
    assert(communicator1.state.members === Map(node1 -> 0))

    // communicator1.leaveGroup()
    // assert(communicator1.state === null)
    // assert(nameserver.listGroups.isEmpty)
  }

  "Leaders" should "be able to leave groups" in {
    val node1 = new NodeID("node1", host, port)
    val node2 = new NodeID("node2", host, port)
    val node3 = new NodeID("node3", host, port)
    val communicator1 =
      BasicGroup.create(group, logger, nameserver,
                        node1, communication, ordering, {msg =>})
    val communicator2 =
      BasicGroup.create(group, logger, nameserver,
                        node2, communication, ordering, {msg =>})
    val communicator3 =
      BasicGroup.create(group, logger, nameserver,
                        node3, communication, ordering, {msg =>})

    assert(communicator1.state.members === communicator2.state.members)
    assert(communicator3.state.members === Map(node1 -> 0, node2 -> 1,
                                               node3 -> 2))

    communicator1.leaveGroup()
    assert(communicator2.state.members === communicator3.state.members)
    assert(communicator3.state.members === Map(node2 -> 1, node3 -> 2))
    assert(communicator2.state.leader === node2)
    assert(communicator3.state.leader === node2)

    val node4 = new NodeID("node4", host, port)
    val communicator4 =
      BasicGroup.create(group, logger, nameserver,
                        node4, communication, ordering, {msg =>})

    assert(communicator2.state.members === communicator3.state.members)
    assert(communicator3.state.members === communicator4.state.members)
    assert(communicator4.state.members === Map(node2 -> 1, node3 -> 2,
                                               node4 -> 3))
    assert(communicator4.state.leader === node2)
  }

  "Counters" should "be incrementable" in {
    val node1 = new NodeID("node1", host, port)
    val node2 = new NodeID("node2", host, port)
    val node3 = new NodeID("node3", host, port)
    val communicator1 =
      BasicGroup.create(group, logger, nameserver,
                        node1, communication, ordering, {msg =>})
    val communicator2 =
      BasicGroup.create(group, logger, nameserver,
                        node2, communication, ordering, {msg =>})
    val communicator3 =
      BasicGroup.create(group, logger, nameserver,
                        node3, communication, ordering, {msg =>})

    communicator1.incCounter()
    assert(communicator2.state.msgCounter === 1)
    assert(communicator3.state.msgCounter === 1)

    communicator2.incCounter()
    assert(communicator1.state.msgCounter === 2)
    assert(communicator3.state.msgCounter === 2)

    communicator3.incCounter()
    assert(communicator1.state.msgCounter === 3)
    assert(communicator2.state.msgCounter === 3)
  }

  "Groups" should "be killable" in {
    val node1 = new NodeID("node1", host, port)
    val node2 = new NodeID("node2", host, port)
    val node3 = new NodeID("node3", host, port)
    val communicator1 =
      BasicGroup.create(group, logger, nameserver,
                        node1, communication, ordering, {msg =>})
    val communicator2 =
      BasicGroup.create(group, logger, nameserver,
                        node2, communication, ordering, {msg =>})
    val communicator3 =
      BasicGroup.create(group, logger, nameserver,
                        node3, communication, ordering, {msg =>})

    assert(communicator1.state.members === communicator2.state.members)
    assert(communicator3.state.members === Map(node1 -> 0, node2 -> 1,
                                               node3 -> 2))

    communicator3.killGroup()
    assert(communicator1.state === null)
    assert(communicator2.state === null)
    assert(communicator3.state === null)
  }

  "Group operations" should "be robust under non-determinism" in {
    val node1 = new NodeID("node1", host, port)
    val node2 = new NodeID("node2", host, port)
    val node3 = new NodeID("node3", host, port)
    val node4 = new NodeID("node4", host, port)
    val node5 = new NodeID("node5", host, port)
    val communicator1 =
      BasicGroup.create(group, logger, nameserver,
                        node1, communication, ordering, {msg =>})
    var communicator2 : BasicGroup = null
    var communicator3 : BasicGroup = null
    var communicator4 : BasicGroup = null
    var communicator5 : BasicGroup = null

    def makeRunnable(assign : (BasicGroup => Unit), node : NodeID)
    : Runnable = new Runnable {
      def run() {
        assign(BasicGroup.create(group, logger, nameserver,
                                 node, communication, ordering, {msg =>}))
      }
    }

    val t2 = new Thread(makeRunnable({ g => communicator2 = g }, node2))
    val t3 = new Thread(makeRunnable({ g => communicator3 = g }, node3))
    val t4 = new Thread(makeRunnable({ g => communicator4 = g }, node4))
    val t5 = new Thread(makeRunnable({ g => communicator5 = g }, node5))
    t2.start(); t3.start(); t4.start(); t5.start();
    t2.join(); t3.join(); t4.join(); t5.join();

    assert(communicator1.state.members === communicator2.state.members)
    assert(communicator3.state.members === communicator4.state.members)
    assert(communicator4.state.members === communicator5.state.members)
    assert(communicator1.state.leader === node1)
    assert(communicator2.state.leader === node1)
    assert(communicator3.state.leader === node1)
    assert(communicator4.state.leader === node1)
    assert(communicator5.state.leader === node1)
  }

  "Groups" should "be lockable" in {
    val node1 = new NodeID("nd1", host, port)
    val node2 = new NodeID("nd2", host, port)
    val node3 = new NodeID("nd3", host, port)
    val node4 = new NodeID("nd4", host, port)

    val lockedGroup = Group("default", UnreliableMulticast(), NoOrdering(), 3)
    
    val communicator1 =
      BasicGroup.create(lockedGroup, logger, nameserver,
                        node1, communication, ordering, {msg =>})
    
    assert(communicator1.isLocked() === true)
    val communicator2 =
      BasicGroup.create(lockedGroup, logger, nameserver,
                        node2, communication, ordering, {msg =>})
    assert(communicator1.isLocked() === true)
    assert(communicator2.isLocked() === true)
    val communicator3 =
      BasicGroup.create(lockedGroup, logger, nameserver,
                        node3, communication, ordering, {msg =>})


    
    assert(communicator1.isLocked() === false)
    assert(communicator2.isLocked() === false)
    assert(communicator3.isLocked() === false)

    intercept[RuntimeException] {
      val communicator4 =
        BasicGroup.create(lockedGroup, logger, nameserver,
                          node4, communication, ordering, {msg =>})
    }

    communicator3.leaveGroup()
    assert(communicator1.state != null)
    assert(communicator2.state != null)
    assert(communicator3.state === null)
  }

}
