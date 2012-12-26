
import org.scalatest._
import scala.util.Random


class testOrderingModules extends FunSuite {
  def createSimpleMessage(msg: DataMessage[String]) = IM( SimpleMessage(Node("f", 0, "n")), msg) 
  //def createMessageFrom()
  
  test("Unordered Queue should hold messages"){
    val q = new UnorderedQueue[String]();
    for(i <- 1 to 10 ) q.insert(createSimpleMessage(DM("A Long Data" + i)));
    assert(q.getAll.length === 10)
  }
  
  def createFIFOMsg(seq: Int, value: String) = createSimpleMessage(SeqM(seq,value))
  
  test("FIFOQueue should deliver messages in correct order"){
    val q = new FIFOQueue[String]();
    val rand = new Random();
    for(i <- rand.shuffle(Range(0,10))) q insert createFIFOMsg(i, "l" + i);
    assert(q.getAll.toSeq === Range(0,10).map("l" + _))
  }
  
}