package expression

import context.Environment
import value.Value
import context.TypeException
import value.Boole
import value.Notification

case class Conditional(val cond:Expression, val cons: Expression, val alt:Expression=null) extends SpecialForm {
  override def execute(env:Environment):Value = {
    val condVal = cond.execute(env)
    if (condVal.isInstanceOf[Boole]) throw new TypeException("Condition need to be Boole")
    if (condVal.asInstanceOf[Boole].value) cons.execute(env)
    else if (alt!=null) alt.execute(env)
    else Notification.UNSPECIFIED
  }
}