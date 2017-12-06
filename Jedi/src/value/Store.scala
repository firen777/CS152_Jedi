package value

import collection.mutable._
import context._

/**contain any number of values like a List<br>
 * extends value.Value
 */
class Store extends Value {
  private var elems = ArrayBuffer[Value]()
  
  /**adds e to the end of store
   * @param e Value to be added.
   */
  def add(e: Value) {elems += e}
  
  /**inserts e at position pos in this<br>
   * make e occupy pos and push the rest of the elements forward.
   * @param e Value to be added 
   * @param pos Position
   */
  def put(e: Value, pos: Integer) {elems.insert(pos.value, e)}
  
  /**removes element at position pos from this
   * @param pos Position
   */
  def rem(pos: Integer) {elems.remove(pos.value)}

  /**returns element at position pos in this
   * @param pos Position
   * @return Value
   */
  def get(pos: Integer): Value = {elems(pos.value)}
  
  /**returns true if this contains e
   * @param e Element to check
   * @return if in Store or not
   */
  def contains(e: Value): Boole = {Boole(elems.exists(_==e))}
  
  /**returns the size of this
   * @return size of Store
   */
  def size: Integer = {Integer(elems.size)}
  
  /**toString function
   * @return "{e0 e1 e2 ...}"
   */
  override def toString = {elems.toString}
  
  
  /**returns store containing the elements of this transformed by trans<br>
   * Just like map function
   * @param trans
   * @return
   */
  def map(trans: Closure): Store = {
    val newStore = new Store
    elems.foreach(x=>{newStore.add(trans(List(x)))})
    newStore
  }
  

  /**returns store containing the elements of this that passed test<br>
   * Just like filter function
   * @param test
   * @return
   */
  def filter(test: Closure): Store = {
    def filterHelp(x: Value): Boolean = {
      val result = test(List(x))
      if (!result.isInstanceOf[Boole])
        throw new TypeException("Expect Closure with return type: Boole")
      else
        result.asInstanceOf[Boole].value
    }
    val newStore = new Store
    elems.foreach(x=>{if (filterHelp(x)) newStore.add(x)})
    newStore
  
  }
}