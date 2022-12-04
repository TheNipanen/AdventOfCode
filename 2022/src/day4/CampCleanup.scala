package day4

import scala.io.Source

object CampCleanup extends App {
  val s = Source.fromFile("src/day4/input.txt")
  val lines = try s.getLines().toArray.map( _.split(",").map( _.split("-").map( _.toInt ) ) ) finally s.close()
  
  var containments = 0
  var overlaps = 0
  var i = 0
  while (i < lines.length) {
    val pair = lines(i)
    val firstStart = pair(0)(0)
    val firstEnd = pair(0)(1)
    val secondStart = pair(1)(0)
    val secondEnd = pair(1)(1)
    if ((firstStart <= secondStart && firstEnd >= secondEnd) || (secondStart <= firstStart && secondEnd >= firstEnd)) {
      containments += 1
    }
    if (!(firstEnd < secondStart || secondEnd < firstStart)) {
      overlaps += 1
    }
    i += 1
  }
  
  println("Containments: " + containments)
  println("Overlaps: " + overlaps)
}