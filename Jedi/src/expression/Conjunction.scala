package expression

import context.Environment
import value.Boole
import context.TypeException

case class Conjunction (val operands: List[Expression]) extends SpecialForm{
  def execute(env: Environment) = {
    if (operands.filter(_.isInstanceOf[Boole]).length != operands.length) 
      throw new TypeException("Conjunction inputs must be Boole")
    def eval(head: Expression, tail: List[Expression], flag: Boolean) = {
     
    }
    
    eval (operands(0), operands.tail, true)
    // execute operands left to right until the answer is known
    // type check and throw exception if one of them is not Boole
    // return false when one of them is false
    null
  }
}