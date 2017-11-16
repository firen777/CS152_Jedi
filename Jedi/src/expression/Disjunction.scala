package expression

import context.Environment
import value.Value
import value.Boole
import context.TypeException

case class Disjunction(val exps:List[Expression]) extends SpecialForm {
  override def execute(env: Environment):Value = {
    if (exps.filter(_.isInstanceOf[Boole]).length != exps.length) 
      throw new TypeException("Disjunction inputs must be Boole")
    def eval(head: Expression, tail: List[Expression]):Value = {
      if (tail == Nil) head.execute(env) //base case, one element only. return execute
      if (!head.execute(env).asInstanceOf[Boole].value) 
        Boole(false)
      eval(tail(0), tail.tail)
      
    }
    
    eval (exps(0), exps.tail)
    
  }
}