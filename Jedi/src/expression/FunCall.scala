package expression

import context.Environment
import value.Value
import context.alu

/**extends expression.Expression
 * @param operator Identifier
 * @param operands List[Expression]
 */
case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression{
  
  /** eager execute operands to produce arguments<br>
   *  then ask ALU to execute operator with arguments
 * @param env Environment table
 * @return Value ALU result
 */
  def execute (env: Environment) : Value = {
    val args = operands.map(_.execute(env))
    alu.execute(operator, args)
  }
}