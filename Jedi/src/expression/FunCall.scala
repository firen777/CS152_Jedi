package expression

import context.Environment
import value.Value
import context.alu

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression{
  def execute (env: Environment) : Value = {
    // eager execute operands to produce arguments
    // ask ALU to execute operator with arguments
    val args = operands.map(_.execute(env))
    alu.execute(operator, args)
  }
}