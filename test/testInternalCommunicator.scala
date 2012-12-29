import org.scalatest._
import java.rmi.registry.LocateRegistry
import java.rmi.server.UnicastRemoteObject

class testComModule extends FunSuite with BeforeAndAfter with BeforeAndAfterAll  {
  
  val message = "A long Data String";

  val PORTNUMBER = 33333;
  
  //Create Registry;
  var registry = LocateRegistry.createRegistry(PORTNUMBER);
  //Create The objects;
  var errors = 0;
  val callback = {h: List[Node] => errors += 1}
  var modules = List(
      new NonReliableCom[String](1, callback),
      new NonReliableCom[String](2, callback),
      new NonReliableCom[String](3, callback)
      );
      
  val nodes = modules map {_.me}
  //Bind the objects to registry;
  modules.foreach({
    module => registry.rebind(
        module.me.id.toString,
        UnicastRemoteObject.exportObject(module.asInstanceOf[Receiver],0));
  });
  
  before {
	  //Reset the nodes
  }
  
  override def afterAll {
    UnicastRemoteObject.unexportObject(registry, true);
  }
  
  test("Sent messages should be recieved by all recievers"){
    modules.head.send(nodes,DM(message));
    
    val result = modules map({ m =>
      m.get().dm.d;
    })
    assert(result === List(message,message,message) );
  }
	
  test("Afterwards no module should have any messages in queue"){
    val result = modules map({ m =>
      m.poll();
    })
    assert(result === List(false,false,false))
  }
}


