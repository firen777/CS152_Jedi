package expression

import context.Environment
import value.Boole
import context.TypeException
import value.Value

case class Conjunction (val exps: List[Expression]) extends SpecialForm{
  override def execute(env: Environment):Value = {
    // execute operands left to right until the answer is known
    // type check and throw exception if one of them is not Boole
    // return false when one of them is false
    for (e <- exps) {
      if (!e.execute(env).isInstanceOf[Boole]) 
        throw new TypeException("Conjunction inputs must be Boole")
      if (!e.execute(env).asInstanceOf[Boole].value) 
        return Boole(false)
    }
    return Boole(true)
  }
}