package day3
import scala.io.Source

object Lobby extends App {
  val s = Source.fromFile("src/day3/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  var totalJoltage = 0
  var totalJoltage2 = 0L
  for (line <- lines) {
    val l = line.size
    
    var maximumJoltage = 0
    var i = 0
    while (i < l) {
      var j = i + 1
      while (j < l) {
        val joltage = ("" + line(i) + line(j)).toInt
        maximumJoltage = maximumJoltage max joltage
        j += 1
      }
      i += 1
    }
    totalJoltage += maximumJoltage
    
    def inner(current: String, i: Int): Long = {
      if (current.size == 12) {
        return current.toLong
      }
      
      var nextI = i + 1
      var checkI = i + 1
      while (checkI <= l - (12 - current.size)) {
        if (line(checkI) > line(nextI)) {
          nextI = checkI
        }
        checkI += 1
      }
      return inner(current + line(nextI), nextI)
    }
    val maximumJoltage2 = inner("", -1)
    totalJoltage2 += maximumJoltage2
  }
  
  println(totalJoltage)
  println(totalJoltage2)
}