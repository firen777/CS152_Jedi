package expression

import context.Environment
import value.Value
import context.TypeException
import value.Notification

/**extends expression.SpecialForm<br>
 * @param id Identifier
 * @param exp Expression
 */
case class Declaration (val id:Identifier, val exp:Expression) extends SpecialForm {
  
  /** Attempt to store id-exp pair in Environment table<br>
   *  throw exception if id already defined
   * @throws TypeException
   * @param env Environment table
   * @return Value Notification.OK
   */
  override def execute(env: Environment):Value = {
    //see if Identifier is already defined in env 
    if (env.isDefinedAt(id)) throw new TypeException(id + " already defined")
    
    env.put(id, exp.execute(env))
    Notification.OK
  }
}