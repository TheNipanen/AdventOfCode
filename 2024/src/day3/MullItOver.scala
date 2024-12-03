package day3

import scala.io.Source

object MullItOver extends App {
  val s = Source.fromFile("src/day3/input.txt")
  val line = try s.getLines().toArray.mkString("") finally s.close()
  
  val regEx = """mul\((\d+),(\d+)\)""".r
  var sum = 0
  for (s <- regEx.findAllMatchIn(line)) {
    val first = s.group(1).toInt
    val second = s.group(2).toInt
    sum += first * second
  }
  
  println(sum)
  
  var conditionalSum = 0
  var enabled = true
  var left = line
  while (left.length() > 0) {
    var nextPoint = left.indexOf("do")
    if (nextPoint == -1) {
      nextPoint = left.length()
    }
    
    if (enabled) {
      val next = left.take(nextPoint)
      for (s <- regEx.findAllMatchIn(next)) {
        val first = s.group(1).toInt
        val second = s.group(2).toInt
        conditionalSum += first * second
      }
    }
    
    if (left.drop(nextPoint).take(4) == "do()") {
      enabled = true
    } else if (left.drop(nextPoint).take(7) == "don't()") {
      enabled = false
    }
    
    left = left.drop(nextPoint + 2)
  }
  
  println(conditionalSum)
}