package value

import collection.mutable._
import context._

/**extends value.Value 
 * 
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
  override def toString = {???}
  // returns store containing the elements of this transformed by trans
  def map(trans: Closure): Store = {???}
  // returns store containing the elements of this that passed test
  def filter(test: Closure): Store = {???}
}