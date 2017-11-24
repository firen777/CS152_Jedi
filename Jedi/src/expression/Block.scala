package expression

import context.Environment
import value.Value

/**extends expression.SpecialForm
 * @param expressions List of Expression to be executed
 */
case class Block (val expressions:List[Expression]) extends SpecialForm{
  
  /**execute the List of Expression relative to the local environment
   * @param env Parent Environment table
   * @return Value last value of the Block
   */
  def execute(env:Environment):Value = {
    val localEnv = new Environment(env)
    
    val executed = expressions.map(_.execute(localEnv))
    executed.last
  }
}