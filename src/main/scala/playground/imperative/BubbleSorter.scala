package playground.imperative

import playground.Sorter

import scala.annotation.tailrec

object BubbleSorter extends Sorter {

  override type A = Int

  override def sort(array: Array[A])(compare: (A, A) => Int): Unit = {
    var i = array.length
    while (i >= 0) {
      var j = i
      while (j < array.length) {
        if ((j != array.length - 1) && compare(array(j), array(j + 1)) > 0) {
          val temp = array(j)
          array(j) = array(j + 1)
          array(j + 1) = temp
        }
        j += 1
      }
      i -= 1
    }
  }

  def recursiveSort(array: Array[Int])(compare: (Int, Int) => Int): Unit = {
    @tailrec
    def loop(end: Int): Unit = {
      if (end != 0) {
        var i = 0
        while (i < end) {
          if ((i != end - 1) && compare(array(i), array(i + 1)) > 0) {
            val temp = array(i)
            array(i) = array(i + 1)
            array(i + 1) = temp
          }
          i += 1
        }
        loop(end - 1)
      }
    }
    loop(array.length)
  }

}
