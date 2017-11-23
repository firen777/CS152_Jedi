package expression

import context.Environment
import value.Value

/**extends expression.SpecialForm
 * @param expressions 
 */
case class Block (val expressions:List[Expression]) extends SpecialForm{
  
/** 
 *  
 *  @throws
 * @param env Parent Environment table
 * @return Value
 */
def execute(env:Environment):Value = {
    val localEnv = new Environment(env)
    // iterate through expressions executing each one relative to localEnv
    // return last value
    
    null
  }
}