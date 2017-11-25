package expression

import context.Environment
import value.Value
import context.alu
import context.UndefinedException
import value.Closure
import context.TypeException

/**extends expression.Expression
 * @param operator Identifier
 * @param operands List[Expression]
 */
case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression{
  
  /**eager execute operands to produce arguments<br>
   * then ask ALU to execute operator with arguments<br>
   * if ALU does not support operation, ask env instead.
   * @throws UndefinedException
   * @param env Environment table
   * @return Value result
   */
  override def execute (env: Environment) : Value = {
    val args = operands.map(_.execute(env))
    
    try {
      alu.execute(operator, args)
    } catch {
      case e: UndefinedException =>  {
        val closureTemp = operator.execute(env)
        //if operator.execute is a closure, call closure (args).
        if (closureTemp.isInstanceOf[Closure]) closureTemp.asInstanceOf[Closure](args)
        else throw e
      }
    }
  }
}