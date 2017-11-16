package expression

import context.Environment
import value.Value

trait Expression {
  /**
 * @param env
 * @return
 */
def execute (env: Environment) : Value
}