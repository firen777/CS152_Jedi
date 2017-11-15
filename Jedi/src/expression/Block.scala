package expression

import context.Environment
import value.Value

case class Block (val expressions:List[Expression]) extends SpecialForm{
  
  def execute(env:Environment):Value = {
    val localEnv = new Environment(env)
    // iterate through expressions executing each one relative to localEnv
    // return last value
    
    null
  }
}