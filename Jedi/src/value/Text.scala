package value

import expression.Literal

/**extends expression.Literal with Ordered[Text] with Equals
 * @param value String
 */
case class Text(val value:String) extends Literal with Ordered[Text] with Equals {
  //concatenation
  def +(other: Text) = Text(this.value + other.value)
  
  def substring(i:Integer, j:Integer)= Text(this.value.substring(i.value, j.value))

  override def toString = value
  
  def compare(other: Text): Int = {
    if (this.value < other.value) -1 
    else if (this.value > other.value) 1 
    else 0
  }
  //canonical form:
  override def canEqual (other: Any) = other.isInstanceOf[Text]
  
  override def equals (other: Any):Boolean = other match {
    case other:Text => this.canEqual(other) && (other.value == this.value)
    case _ => false
  }
  override def hashCode = this.##
  
}
