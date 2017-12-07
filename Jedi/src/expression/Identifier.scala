package expression

import context.Environment
import context.UndefinedException

/**extends expression.Expression 
 * @param name String
 * @param names List of String. Qualified Names.
 */
case class Identifier (val name:String, val names: List[String] = Nil) extends Expression {
  override def toString = name
  /** Execute id in Environment table as well as parent chain of tables<br>
   *  throw UndefinedException if not in the chain of tables
   *  @throws UndefinedException
   * @param env Environment table
   * @return Value result
   */
  override def execute(env: Environment) = {
//    env.get(this) match {
//      case None => throw new UndefinedException (this)
//      case Some(value) => value
//    }
    if (names == Nil) env(this)
    else 
      ???
  }
}