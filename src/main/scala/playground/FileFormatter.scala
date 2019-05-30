package playground

import java.lang.Math.max

import scala.io.Source

object FileFormatter {

  def format(filename: String): Unit = {
    val lines = Source.fromFile(filename).getLines().toList

    val maxWidth = lines.map(widthOfLength).reduceLeft(max)

    lines.foreach(line => println(s"${" " * (maxWidth - widthOfLength(line))}${line.length}| $line"))
  }

  def widthOfLength(s: String): Int = s.length.toString.length

}
