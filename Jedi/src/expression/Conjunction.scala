package expression

import context.Environment
import value.Boole
import context.TypeException
import value.Value

/**extends expression.SpecialForm
 * @param exps List of Expression//Boole
 * 
 */
case class Conjunction (val exps: List[Expression]) extends SpecialForm{
  
  /** execute operands left to right until the answer is known<br>
   *   type check and throw exception if one of them is not Boole
   *   return false when one of them is false
   *   @throws TypeException
 * @param env Environment table
 * @return Value Boole(true) or Boole(false)
 */
override def execute(env: Environment):Value = {
    for (e <- exps) {
      if (!e.execute(env).isInstanceOf[Boole]) 
        throw new TypeException("Conjunction inputs must be Boole")
      if (!e.execute(env).asInstanceOf[Boole].value) 
        return Boole(false)
    }
    return Boole(true)
  }
}