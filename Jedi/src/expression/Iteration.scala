package expression

import context.Environment
import value.Value

/**while (condition) {body}<br>
 * extends expression.SpecialForm
 * @param condition
 * @param body
 */
case class Iteration(val condition:Expression, val body:Expression) extends SpecialForm {
  override def execute(env:Environment):Value = {
    null
  }
  
}