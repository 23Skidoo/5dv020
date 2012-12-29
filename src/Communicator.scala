//TODO Implement this class!

/**
 * Communicator should be the class exposed to the user of this middleware
 *
 */
class Communicator[T](
    val id: Int,
    val onRecv: T => Unit,
    val internal_com: InternalCommunicator[T],
    val order: OrderingModule[T]) {
  val me = new RefNode(internal_com, id)
  /*
   * broadCast
   * leave
   * */
}