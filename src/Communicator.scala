//TODO Implement this class!


class Communicator[T](
		/*
		 * Communicator should be the class exposed to the user of this middleware
		 * Should contain all three modules of the middleware:
		 *   GroupModule
		 *   OrderingModule
		 *   CommunicationModule
		 *   
		 *   Communicator should start a thread watching over the internal_coms 
		 *   incoming queue and fire off the onRecv callback
		 */
    val id: Int,
    val onRecv: T => Unit,
    val group: Group
    //val groupcommunication: GroupModule  
  ) {

  //TODO build GroupCom here
    
    //TODO these should be provided by groupCom
    val onErrorPlaceHolder = {p: List[Node] => } 
    val hostCallBackHolder = {() => List[Node]() } 
    
    val internalCom = group match {
      case Group(_,comType: NonReliable, _) => new NonReliableCom[T](id, onErrorPlaceHolder)
      case Group(_,comType: Reliable, _) => new ReliableCom[T](id, onErrorPlaceHolder, hostCallBackHolder)
    }
    val node = RefNode(internalCom,id);
    val order = group match {
      case Group(_,_,ordering: Unordered) => new UnorderedQueue[T]()
      case Group(_,_,ordering: FIFO)      => new FIFOQueue[T]()
      case Group(_,_,ordering: Causal)    => new CausalQueue[T](node)
      //How do we get the  callback to the totalorderqueue?
      //case Group(_,_,ordering: Total) =>     new TotalOrderQueue[T]()
    } 
    val me = new RefNode(internalCom, id)
  /*
   * broadCast(data: T) //Should be something like internalCom.send(order.createMessage(data))
   * leave
   * */
}