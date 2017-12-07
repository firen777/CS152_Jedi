package expression

import context.Environment
import value.Value
import value.Boole
import context.TypeException
import value.Notification

/**while (condition) {body}<br>
 * extends expression.SpecialForm
 * @param condition
 * @param body
 */
case class Iteration(val condition:Expression, val body:Expression) extends SpecialForm {
  
  
  /**While loop<br>
   * Throw TypeException if condition.execute is not Boole
   * @throws TypeException
   * @param env Environment table
   * @return Value Notification.DONE
   */
  override def execute(env:Environment):Value = {

    if (!condition.execute(env).isInstanceOf[Boole])
      throw new TypeException("Condition need to be of type Boole.")
    else
      while (condition.execute(env).asInstanceOf[Boole].value) 
        body.execute(env)

      Notification.DONE
  }
  
}