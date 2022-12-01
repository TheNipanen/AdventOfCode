package day1

import scala.io.Source

object CalorieCounting extends App {
  val s = Source.fromFile("src/day1/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  var max = 0
  var sec = 0
  var thi = 0
  var current = 0
  
  def updateTopThree() = {
    if (current >= max) {
      thi = sec
      sec = max
      max = current
    } else if (current >= sec) {
      thi = sec
      sec = current
    } else if (current >= thi) {
      thi = current
    }
  }
  
  var i = 0
  while (i < lines.length) {
    val line = lines(i)
    if (line == "") {
      updateTopThree()
      current = 0
    } else {
      current += line.toInt
    }
    i += 1
  }
  updateTopThree()
  
  println("First: " + max)
  println("Second: " + sec)
  println("Third: " + thi)
  println("Sum: " + (max + sec + thi))
}