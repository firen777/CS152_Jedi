package expression

import context.Environment
import value.Value

class Obj (val delegate:Identifier=null, val members:List[Declaration]) extends SpecialForm {
  override def execute(env: Environment):Value = {
    ???
  }
}