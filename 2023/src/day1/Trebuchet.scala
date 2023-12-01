package day1

import scala.io.Source
import scala.collection.mutable.ListBuffer

object Trebuchet extends App {
  val s = Source.fromFile("src/day1/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val values = ListBuffer[Int]()
  val values2 = ListBuffer[Int]()
  
  def writtenDigit(line: String, i: Int, start: Boolean): Option[String] = {
		// :DD
    val op = if (start) i.+(_: Int) else i.-(_: Int)
    val ss3 = if (start && op(3) <= line.length) line.substring(i, op(3)) else if (!start && op(3) >= 0) line.substring(op(3), i) else ""
    val ss4 = if (start && op(4) <= line.length) line.substring(i, op(4)) else if (!start && op(4) >= 0) line.substring(op(4), i) else ""
    val ss5 = if (start && op(5) <= line.length) line.substring(i, op(5)) else if (!start && op(5) >= 0) line.substring(op(5), i) else ""
    if (ss3 == "one") Some("1")
    else if (ss3 == "two") Some("2")
    else if (ss5 == "three") Some("3")
    else if (ss4 == "four") Some("4")
    else if (ss4 == "five") Some("5")
    else if (ss3 == "six") Some("6")
    else if (ss5 == "seven") Some("7")
    else if (ss5 == "eight") Some("8")
    else if (ss4 == "nine") Some("9")
    else None
  }
  
  val l = lines.length
  var i = 0
  while (i < l) {
    val line = lines(i)
    var j = 0
    var k = line.length()
    
    var written: Option[String] = None
    while (j < k && !line(j).isDigit) {
      if (written.isEmpty) {
        written = writtenDigit(line, j, true)
      }
      j += 1
    }
    val first = line(j).toString()
    val first2 = written.getOrElse(first)
    
    written = None
    while (k > 0 && !line(k - 1).isDigit) {
      if (written.isEmpty) {
        written = writtenDigit(line, k, false)
      }
      k -= 1
    }
    val last = line(k - 1).toString()
    val last2 = written.getOrElse(last)
    
    values += (first + last).toInt
    values2 += (first2 + last2).toInt
    i += 1
  }
  
  val sum = values.sum
  val sum2 = values2.sum
  println("Sum of calibration values: " + sum)
  println("Sum of calibration values with written digits: " + sum2)
}