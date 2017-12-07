package expression

import context.Environment
import value.Value
import value.Notification
import value.Integer
import value.Variable
import context.TypeException


/**vbl = update <br>
 * extends expression.SpecialForm
 * @param vbl
 * @param update
 */
case class Assignment(val vbl: Identifier, val update: Expression) extends SpecialForm {
  /**Update vbl->var's content in Environment table<br>
   * throws TypeException if not a variable.
   * @throws TypeException 
   * @param env Environment table
   * @return Value Notification.DONE
   */
  override def execute(env:Environment):Value = {
    val v = vbl.execute(env)
    if (!v.isInstanceOf[Variable])
      throw new TypeException("Can only modify Variable.")
    else
      v.asInstanceOf[Variable].content = update.execute(env)
    Notification.DONE
  }
}