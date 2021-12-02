package day2

import scala.io.Source

object Dive extends App {
  val s = Source.fromFile("src/day2/input.txt")
  val commands = s.getLines().toArray.map( _.split(" ") )
  var pos = 0
  var depth = 0
  for (c <- commands) {
    val cc = c(0)
    val amount = c(1).toInt
    cc match {
      case "forward" => pos += amount
      case "down" => depth += amount
      case "up" => depth -= amount
      case _ => println("wot")
    }
  }
  println("Position: " + pos)
  println("Depth: " + depth)
  println("Multiplied: " + (pos * depth))
  
  pos = 0
  depth = 0
  var aim = 0
  for (c <- commands) {
    val cc = c(0)
    val amount = c(1).toInt
    cc match {
      case "forward" => {
        pos += amount
        depth += aim*amount
      }
      case "down" => aim += amount
      case "up" => aim -= amount
      case _ => println("wot")
    }
  }
  println("PositionNew: " + pos)
  println("DepthNew: " + depth)
  println("Multiplied: " + (pos * depth))
}