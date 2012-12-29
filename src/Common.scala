
abstract class Reliability
case class Reliable() extends Reliability;
case class NonReliable() extends Reliability;

abstract class Ordering;
case class Unordered() extends Ordering;
case class FIFO() extends Ordering;
case class Causal() extends Ordering;
case class Total() extends Ordering;
case class CausalTotal() extends Ordering;

abstract class JoinGroup;
case class NewGroup(groupName: String, reliability: Reliability, ordering: Ordering) extends JoinGroup;
case class ExistingGroup(groupName: String) extends JoinGroup;

case class Group(groupName: String, reliability: Reliability, ordering: Ordering)

/**
 * A Node in the system, needed so we can have a DummyNode for testing purposes
 */
abstract class Node{
  def ref: Receiver;
  def id: Int;
}

/**
 * Implementation of Node class used by the system
 */
case class RefNode(ref: Receiver, id: Int) extends Node

/**
 * Node used for testing ordering modules.
 */
class DummyNode() extends Node{
  def ref: Receiver = null;
  def a = null;
  def id = -1;
}

 

