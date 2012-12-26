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
  val callback = {h: Node => errors += 1}
  var modules = List(
      new BasicCom[String](Node("localhost",PORTNUMBER,"1"), callback),
      new BasicCom[String](Node("localhost",PORTNUMBER,"2"), callback),
      new BasicCom[String](Node("localhost",PORTNUMBER,"3"), callback)
      );
      
  val nodes = modules map {_.me}
  //Bind the objects to registry;
  modules.foreach({
    module => registry.rebind(
        module.me.name,
        UnicastRemoteObject.exportObject(module.asInstanceOf[Receiver[String]],0));
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


