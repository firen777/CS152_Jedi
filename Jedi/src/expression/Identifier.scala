package expression

import context.Environment

case class Identifier (val name:String) extends Expression {
  override def toString = name
  override def execute(env: Environment) = {
    env.get(this) match {
      case None => throw new Exception ("undefined: " + name)
      case Some(value) => value
    }
  }
}