package value

import expression.Identifier
import expression.Expression
import context.Environment

class Closure(params:Identifier, body:Expression, defEnv:Environment) extends Value {
  
  def apply(ars:List[Value], callEnv:Environment=null):Value = {
    null
    /* 1.create tempEnv extending def Env //extra credit: callingEnv if DynamicFlag = true
     * 2.bulk put params = args in tempEnv
     * 3.body.exec(tempEnv)
     */
    
  }
}