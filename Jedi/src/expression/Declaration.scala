package expression

import context.Environment
import value.Value
import context.TypeException
import value.Notification

case class Declaration (val id:Identifier, val exp:Expression) extends SpecialForm {
  override def execute(env: Environment):Value = {
    //see if Identifier is already defined in env 
    if (env.isDefinedAt(id)) throw new TypeException(id + " already defined")
    
    env.put(id, exp.execute(env))
    Notification.OK
  }
}