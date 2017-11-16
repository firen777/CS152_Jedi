package expression
import value.Value
import context.Environment

/**
 * extends expression.Expression with value.Value
 *
 */
trait Literal extends Expression with Value{
  def execute(env: Environment) = this
}