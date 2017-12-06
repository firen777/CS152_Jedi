package expression

import context.Environment
import value.Value
import value.Notification


/**id = exp <br>
 * extends expression.SpecialForm
 * @param vbl
 * @param update
 */
case class Assignment(val id: Identifier, val exp: Expression) extends SpecialForm {
  /** Update id-exp pair in Environment table<br>
   * @param env Environment table
   * @return Value Notification.DONE
   */
  override def execute(env:Environment):Value = {
    env.put(id, exp.execute(env))
    Notification.DONE
  }
}