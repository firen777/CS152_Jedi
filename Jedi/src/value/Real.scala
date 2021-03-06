package value

import expression.Literal

/**extends expression.Literal with Ordered[Real] with Equals
 * @param value Double
 */
case class Real(val value:Double) extends Literal with Ordered[Real] with Equals {
  def +(other: Real) = Real(this.value + other.value)
  def -(other: Real) = Real(this.value - other.value)
  def *(other: Real) = Real(this.value * other.value)
  def /(other: Real) = Real(this.value / other.value)

  override def toString = value.toString
  
  def compare(other: Real): Int = {
    if (this.value < other.value) -1 
    else if (this.value > other.value) 1 
    else 0
  }
  //canonical form:
  override def canEqual (other: Any) = other.isInstanceOf[Real]
  
  override def equals (other: Any):Boolean = other match {
    case other:Real => this.canEqual(other) && (other.value == this.value)
    case _ => false
  }
  override def hashCode = this.toString.##
  
}

object Real {
  implicit def realToInt(n:Real): Integer = Integer(n.value.toInt)
}