package expression

import value.Closure
import context.Environment
import value.Value

class Lambda(params:Identifier, body:Expression) {
  def execute(env:Environment):Value = {
    new Closure(params, body, env)
  }
}