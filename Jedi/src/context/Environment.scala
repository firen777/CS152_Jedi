package context

import scala.collection.mutable._
import value._
import expression._
/**Hash Map for value
 * @param extension extension to parent block's Environment. Default null.
 */
class Environment(var extension: Environment = null)
   extends HashMap[Identifier, Value] with Value {

  // used by closures to bind parameters to arguments
  /**Construct Identifier <-> Value pairs in the table.<br>
   * Throw Exception if both List are different length
   * @throws TypeException
   * @param params List of Identifier
   * @param args List of Value
   */
  def bulkPut(params: List[Identifier], args: List[Value]) {
    if (params.length != args.length) throw new TypeException("# arguments != #parameters")
    for(i <- 0 until params.length) this.put(params(i), args(i))
  }
  
  override def apply(name: Identifier): Value = {
    if (this.contains(name)) super.apply(name)
    else if (extension != null) extension.apply(name)
    else throw new UndefinedException(name)
  }
}