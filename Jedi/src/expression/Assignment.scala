package expression

import context.Environment
import value.Value


/**vbl = update <br>
 * extends expression.SpecialForm
 * @param vbl
 * @param update
 */
case class Assignment(val vbl: Identifier, val update: Expression) extends SpecialForm {
  override def execute(env:Environment):Value = {
    null
  }
}