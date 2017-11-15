package value

class Notification (val acknowledgement: String) extends Value{
  override def toString = acknowledgement
}

object Notification {
  def apply(acknowledgement: String) = new Notification(acknowledgement)
  val OK = Notification("OK")
  val DONE = Notification("DONE")
  val UNSPECIFIED = Notification("UNSPECIFIED")
}