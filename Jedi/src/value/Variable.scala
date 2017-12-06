package value

/**extends value.Value
 * @param content
 */
class Variable (val content: Value) extends Value{
  /**
   * @return "[%s]" where %s is content.toString
   */
  override def toString = "[" + content.toString + "]"
  
  def deRef = content
}