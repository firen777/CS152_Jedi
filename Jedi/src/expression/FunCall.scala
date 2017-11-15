package expression

import context.Environment
import value.Value

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression{
  def execute (env: Environment) : Value = {
    // eager execute operands to produce arguments
    // ask ALU to execute operator with arguments
    null
  }
}