package expression

import context.Environment
import value.Value
import context.TypeException
import value.Boole
import value.Notification

/**extends expression.SpecialForm
 * @param cond Condition
 * @param cons Consequence
 * @param alt Alternative
 */
case class Conditional(val cond:Expression, val cons: Expression, val alt:Expression=null) extends SpecialForm {
  
  /** Given condition, execute consequence or alternative <br>
   *  type check and throw exception if cond is not Boole
   *  @throws TypeException
 * @param env Environment table
 * @return Value consequence or alternative or Notification.UNSPECIFIED
 */
  override def execute(env:Environment):Value = {
    val condVal = cond.execute(env)
    if (condVal.isInstanceOf[Boole]) throw new TypeException("Condition need to be Boole")
    if (condVal.asInstanceOf[Boole].value) cons.execute(env)
    else if (alt!=null) alt.execute(env)
    else Notification.UNSPECIFIED
  }
}