package expression

case class Conditional(val cond:Expression, val cons: Expression, val alt:Expression=null) extends SpecialForm {
  
}