package day7

import scala.io.Source

object TheTreacheryOfWhales  extends App {
  val s = Source.fromFile("src/day7/input.txt")
  val positions = s.getLines().toArray.map( _.split(",") ).flatten.map( _.toInt )
  
  var minSum = Int.MaxValue
  var i = 0
  while (i < positions.size) {
    val target = positions(i)
    var j = 0
    var sum = 0
    while (j < positions.size) {
      val current = positions(j)
      sum += math.abs(target - current)
      j += 1
    }
    minSum = minSum min sum
    i += 1
  }
  
  println("Minimum fuel spent: " + minSum)
  
  minSum = Int.MaxValue
  i = 0
  while (i < positions.size) {
    val target = positions(i)
    var j = 0
    var sum = 0
    while (j < positions.size) {
      val current = positions(j)
      val n = math.abs(target - current)
      sum += (n * (n + 1)) / 2
      j += 1
    }
    minSum = minSum min sum
    i += 1
  }
  
  println("New minimum fuel spent: " + minSum)
}