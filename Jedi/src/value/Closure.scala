package value

import expression.Identifier
import expression.Expression
import context.Environment

/**functions that remember their defining environment<br>
 * extends value.Value 
 * @param params List of Identifier
 * @param body Expression
 * @param defEnv Environment
 */
class Closure(params:List[Identifier], body:Expression, defEnv:Environment) extends Value {
  
  
  /**<ul>
   * <li>create tempEnv extending def Env //extra credit: callingEnv if DynamicFlag = true</li>
   * <li>bulk put params <-> args in tempEnv<li/>
   * <li>body.exec(tempEnv)<li/>
   * </ul>
   * @param args arguments of the Closure
   * @param callEnv default null //extra credit, not yet implemented
   * @return body.exec(tempEnv)
   */
  def apply(args:List[Value], callEnv:Environment=null):Value = {
    val tempEnv = new Environment(defEnv)
    tempEnv.bulkPut(params, args)
    body.execute(tempEnv)
  }
}