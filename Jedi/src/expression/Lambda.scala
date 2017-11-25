package expression

import value.Closure
import context.Environment
import value.Value

/**Extends expression.SpecialForm
 * @param params List of Identifier
 * @param body Expression
 */
case class Lambda(params:List[Identifier], body:Expression) extends SpecialForm {
  
  /**
   * @param env Environment table
   * @return Closure(params, body, env)
   */
  def execute(env:Environment):Value = {
    new Closure(params, body, env)
  }
}