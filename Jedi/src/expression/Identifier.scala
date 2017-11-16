package expression

import context.Environment
import context.UndefinedException

/**extends expression.Expression 
 * @param name String
 */
case class Identifier (val name:String) extends Expression {
  override def toString = name
  /** Execute id in Environment table<br>
   *  throw UndefinedException if not in the table
   *  @throws UndefinedException
 * @param env Environment table
 * @return Value result
 */
  override def execute(env: Environment) = {
    env.get(this) match {
      case None => throw new UndefinedException (this)
      case Some(value) => value
    }
  }
}