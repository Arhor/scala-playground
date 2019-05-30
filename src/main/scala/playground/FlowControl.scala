package playground

import scala.annotation.tailrec

object FlowControl {

  @tailrec
  def повторять_пока(condition: => Boolean)(body: => Unit): Unit = {
    if (condition) {
      body
      повторять_пока(condition)(body)
    }
  }

}
