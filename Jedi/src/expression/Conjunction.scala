package expression

import context.Environment
import value.Boole
import context.TypeException
import value.Value

case class Conjunction (val exps: List[Expression]) extends SpecialForm{
  override def execute(env: Environment):Value = {
    if (exps.filter(_.isInstanceOf[Boole]).length != exps.length) 
      throw new TypeException("Conjunction inputs must be Boole")
    def eval(head: Expression, tail: List[Expression]):Value = {
      if (tail == Nil) head.execute(env) //base case, one element only. return execute
      if (!head.execute(env).asInstanceOf[Boole].value) 
        Boole(false)
      eval(tail(0), tail.tail)
      
    }
    
    eval (exps(0), exps.tail)
    // execute operands left to right until the answer is known
    // type check and throw exception if one of them is not Boole
    // return false when one of them is false
    
  }
}