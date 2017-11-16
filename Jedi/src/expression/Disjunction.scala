package expression

import context.Environment
import value.Value
import value.Boole
import context.TypeException

case class Disjunction(val exps:List[Expression]) extends SpecialForm {
  override def execute(env: Environment):Value = {
    for (e <- exps) {
      if (!e.execute(env).isInstanceOf[Boole]) 
        throw new TypeException("Disjunction inputs must be Boole")
      if (e.execute(env).asInstanceOf[Boole].value) 
        return Boole(true)
    }
    return Boole(false)
  }
}