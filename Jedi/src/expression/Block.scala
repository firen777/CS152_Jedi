package expression

import context.Environment
import value.Value

/**extends expression.SpecialForm
 * @param expressions List of Expression to be executed
 */
case class Block (val expressions:List[Expression]) extends SpecialForm{
  
  /**execute the List of Expression relative to the local environment
   * @param env Parent Environment table
   * @return last Value of the Block
   */
  override def execute(env:Environment):Value = {
    val tempEnv = new Environment(env)
    
    val executed = expressions.map(_.execute(tempEnv))
    executed.last
  }
}