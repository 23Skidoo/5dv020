package test.ordering
import java.rmi.registry.LocateRegistry
import org.scalatest.FlatSpec
import org.slf4j.LoggerFactory
import gcom.common._
import gcom.transport._
import gcom.communication._
import gcom.ordering._
import scala.util.Random
import gcom.ordering.Causal



/** Requires an rmiregistry running on port 31337. */
class CausalTotalSpec extends FlatSpec {

  "CausalTotal ordering" should "deliver messages in a causal order" in {
    val host     = "localhost" //Util.getLocalHostName
    val port     = 31337

    val name     = Util.getRandomUUID

    val id       = new NodeID(name, host, port)

    /* Registry must be running. */
    val registry = LocateRegistry.getRegistry(port)

    var receivedList: List[Message] = List()
    var sentMessages: List[String] = List()
    //The first causal message sent will have a time of 1
    val shuffled = Random.shuffle((1 to 3))

    var order = 0;

    val a = NodeID.fromString("1:a:1")
    val b = NodeID.fromString("1:b:1")
    val c = NodeID.fromString("1:c:1")


    val logger        = LoggerFactory.getLogger(id.toString)
    val transport     = BasicTransport.create(id, {msg =>}, logger);
    val communication = NonReliable.create(transport, {msg =>})
    val ordering      =
      CausalTotal.create(
          communication,
          {msg => receivedList = receivedList :+ msg },
          a,
          {() => order += 1; order - 1})
    val thread        = new Thread(transport);
    thread.start();

    ordering.updateView(Map((a,0),
                            (b,0),
                            (c,0)),
                        0)

    val outboundOrder = List(
        c -> (0,0,1,1),
        a -> (1,0,0,0),
        a -> (2,0,0,2),
        b -> (0,1,2,4),
        a -> (3,2,2,6),
        b -> (0,2,2,5),
        c -> (0,0,2,3));
    val expectedOrder = List("1a", "1c", "2a", "2c", "1b", "2b", "3a")
    outboundOrder.map({ case (node,clock) =>
      val payload = if(node == a){
        clock.productElement(0)+"a"
      } else if(node == b){
        clock.productElement(1)+"b"
      } else clock.productElement(2)+"c"

      val testm = TestMessage.create(
          payload.toString, new CausalTotalData(
              Map(a -> clock._1,
                  b -> clock._2,
                  c -> clock._3),
              clock._4));
      testm.addSender(node)
      transport.receiveMessage(testm)
    })

    Thread.sleep(1000)
    assert(receivedList.map(_.payload) === expectedOrder )

    transport.receiveMessage(new BlackSpot())
  }
}
