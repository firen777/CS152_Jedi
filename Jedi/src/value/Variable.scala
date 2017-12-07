package value

/**extends value.Value
 * @param content
 */
class Variable (var content: Value) extends Value{
  /**
   * @return "[%s]" where %s is content.toString
   */
  override def toString = "[" + content.toString + "]"
  
}