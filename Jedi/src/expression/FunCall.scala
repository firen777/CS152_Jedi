package expression

import context.Environment
import value.Value
import context.alu
import context.UndefinedException
import value.Closure

/**extends expression.Expression
 * @param operator Identifier
 * @param operands List[Expression]
 */
case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression{
  
  /** eager execute operands to produce arguments<br>
   *  then ask ALU to execute operator with arguments
 * @param env Environment table
 * @return Value result
 */
  def execute (env: Environment) : Value = {
    try {
      val args = operands.map(_.execute(env))
      alu.execute(operator, args)
    } catch {
      case e: UndefinedException =>  {
        val closureTemp = operator.execute(env)
        if (closureTemp.isInstanceOf[Closure]) closureTemp
        
        //if operator.execute is a closure, call closure (args).
      }
    }
  }
}