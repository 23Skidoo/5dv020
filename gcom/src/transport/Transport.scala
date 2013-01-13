package gcom.transport

import java.rmi.Remote
import java.rmi.RemoteException
import java.rmi.registry.LocateRegistry
import java.rmi.registry.Registry
import java.rmi.server.UnicastRemoteObject
import java.util.concurrent.{LinkedBlockingQueue}
import org.slf4j.Logger
import scala.collection.mutable.HashMap;

import gcom.common._
import scala.swing.Publisher

/** The message transport layer. The only part of the system that knows about
 *  RMI. Made abstract to allow different implementations. Runnable because
 *  it's the part that drives the whole system through callbacks. */
trait Transport extends Remote with Runnable {

  /** Send a message to the given destination. Return Some[dst] in case of
   *  success, and None on error. */
  @throws(classOf[RemoteException])
  def sendMessage(to : NodeID, msg : AbstractMessage) : Option[NodeID];

  /** Used by remote clients to post messages to the transport's
   *  internal queue. */
  @throws(classOf[RemoteException])
  def receiveMessage(msg : AbstractMessage) : Unit;

  /** Change the callback invoked when a message is received. */
  @throws(classOf[RemoteException])
  def setOnReceive(callbck : Message => Unit) : Unit
  
  @throws(classOf[RemoteException])
  def nodeID(): NodeID;

  @throws(classOf[RemoteException])
  def run() : Unit;
}

/** A basic transport layer implementation. */
class BasicTransport(id : NodeID,
                     callbck : Message => Unit, loggr : Logger) 
                   extends Transport with Publisher
{
  val nodeID     = id;
  var callback   = callbck;
  val logger     = loggr;
  val queue      = new LinkedBlockingQueue[AbstractMessage]();
  val registries = new HashMap[(String, Int), Registry]();
  val stubs      = new HashMap[NodeID, Transport]();

  /* If we can't locate registry, dying with an exception is fine. */
  private def locateRegistry(host : String, port : Int) : Registry = {
    registries.get((nodeID.host, nodeID.port)) match {
      case None => { val registry = LocateRegistry.getRegistry(host, port);
                     registries((host, port)) = registry;
                     return registry;
                   }
      case Some(r) => r
    }
  }

  /* It's OK to throw exceptions here: can't locate stub - we're toast. */
  private def locateStub(n : NodeID) : Option[Transport] = {
    stubs.get(n) match {
      case None => { val registry = locateRegistry(n.host, n.port)
                     val stub = registry.lookup(n.name).asInstanceOf[Transport]
                     stubs(n) = stub
                     Some(stub)
                   }
      case s    => s
    }
  }

  private def register() = {
    val stub = UnicastRemoteObject.exportObject(this, 0)
    val registry = locateRegistry(nodeID.host, nodeID.port)
    registry.rebind(nodeID.name, stub)
    logger.debug("Transport ready")
  }

  def setOnReceive(callbck : Message => Unit) = callback = callbck

  def receiveMessage(msg : AbstractMessage) = {
    queue.put(msg);
    publish(UpdateSentMessages(1))
    logger.debug("Message received: " + msg.toString)
  }

  def sendMessage(dst : NodeID, msg : AbstractMessage) : Option[NodeID] = {
    try {
      msg.addSender(nodeID)
      val mstub = locateStub(dst)
      mstub.flatMap { stub =>
        logger.debug("Sending message to: " + dst.toString)
        stub.receiveMessage(msg)
        logger.debug("Message sent: " + msg.toString)
        publish(UpdateSentMessages(1))
        Some(dst)
      }
    }
    catch {
      case _ : RemoteException => None
    }
  }

  def run() : Unit = {
    while (true) {
      val msg = queue.take();
      msg match {
        case BlackSpot()   => return;
        case msg : Message => callback(msg);
      }
    }
  }
}

/** Companion object for BasicTransport. */
object BasicTransport {
  def create(id : NodeID,
             callback : Message => Unit, logger : Logger) : Transport = {
    val t = new BasicTransport(id, callback, logger);
    t.register();
    return t;
  }
}
