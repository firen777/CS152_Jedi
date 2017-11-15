package expression

import context.Environment

case class Conjunction (val operands: List[Expression]) extends SpecialForm{
  def execute(env: Environment) = {
    // execute operands left to right until the answer is known
    // type check and throw exception if one of them is not Boole
    // return false when one of them is false
    null
  }
}