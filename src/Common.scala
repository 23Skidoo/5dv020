

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

case class Node(host: String, port:Int, name:String);
