package day1

import scala.io.Source

object HistorianHysteria extends App {
  val s = Source.fromFile("src/day1/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val sortedLists = lines.map( _.split("\\s+").map( _.toInt ) ).transpose.map( _.sorted )
  val l = sortedLists(0).length
  
  var distance = 0
  var similarity = 0
  var i = 0
  while (i < l) {
    val left = sortedLists(0)(i)
    distance += math.abs(left - sortedLists(1)(i))
    similarity += left * sortedLists(1).count( _ == left )
    i += 1
  }
  
  println(distance)
  println(similarity)
}