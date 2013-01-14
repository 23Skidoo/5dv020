package test.ordering
import org.scalatest._
import scala.util.Random
import gcom.common._
import java.rmi.registry.LocateRegistry
import org.slf4j.LoggerFactory
import gcom.transport.BasicTransport
import gcom.communication.NonReliable
import gcom.ordering.FIFO


class FIFOSpec extends FlatSpec {

  "FIFO ordering" should "order messages from each sender" in {
    val host     = "localhost" //Util.getLocalHostName
    val port     = 31337
    val name     = Util.getRandomUUID
    val id       = new NodeID(name, host, port)

    /* Registry must be running. */
    val registry = LocateRegistry.getRegistry(port)

    var message : Message  = TestMessage.create("BOGUS");
    var receivedMessages = List[(NodeID,Int)]();

    val logger        = LoggerFactory.getLogger(id.toString)
    val transport     = BasicTransport.create(id, {msg =>}, logger);
    val communication = NonReliable.create(transport, {msg =>})
    val ordering      = FIFO.create(communication, {msg =>
      receivedMessages = receivedMessages :+
        (msg.senders.head -> msg.payload.toInt)
      })
    val thread        = new Thread(transport);
    thread.start();
    val a = NodeID.fromString("1:a:1")
    val b = NodeID.fromString("1:b:1")
    val c = NodeID.fromString("1:c:1")

    // c:1 a:3 a:2 b:1 a:1 b:2 c:2 should result in c:1 b:1 a:1-3 b:2 c:2
    // a -> 0 should be "lost"
    val outboundOrder = List(c -> 1, a -> 1, a -> 2, b -> 1, a -> 0, a -> 1, b -> 2, c -> 2);
    val expectedOrder = List(c -> 1, a -> 1, a -> 2, b -> 1, b -> 2, c -> 2)
    outboundOrder.map({ m =>
      val testm = TestMessage.create(""+m._2, new FIFOData(m._2));
      testm.addSender(m._1)
      transport.receiveMessage(testm)
    })


    Thread.sleep(1000)
    assert(receivedMessages === expectedOrder)

    transport.receiveMessage(new BlackSpot())
  }

}
