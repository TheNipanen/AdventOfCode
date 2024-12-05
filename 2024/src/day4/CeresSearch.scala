package day4

import scala.io.Source

object CeresSearch extends App {
  val s = Source.fromFile("src/day4/input.txt")
  val lines = try s.getLines().toList finally s.close()
  
  val length = lines.length
  val lineLength = lines(0).length
  
  def charInCoords(x: Int, y: Int): Char = {
    if (x >= 0 && x < length && y >= 0 && y < lineLength) {
      lines(x)(y)
    } else {
      ' '
    }
  }
  
  val increments = Array((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
  def findXmas(i: Int, j: Int): Int = {
    val stringsInDirections = Array("", "", "", "", "", "", "", "")
    for (dist <- (0 until 4)) {
      var dir = 0
      while (dir < stringsInDirections.length) {
        val incr = increments(dir)
        val c = charInCoords(i + dist * incr._1, j + dist * incr._2)
        stringsInDirections(dir) += c
        dir += 1
      }
    }
    stringsInDirections.count( _ == "XMAS" )
  }
  
  def findX(i: Int, j: Int): Boolean = {
    val here = charInCoords(i, j)
    val first = "" + charInCoords(i-1, j-1) + here + charInCoords(i+1, j+1)
    val second = "" + charInCoords(i-1, j+1) + here + charInCoords(i+1, j-1)
    (first == "MAS" || first == "SAM") && (second == "MAS" || second == "SAM")
  }
  
  var count = 0
  var countX = 0
  var i = 0
  while (i < length) {
    var j = 0
    while (j < lineLength) {
      val startingHere = findXmas(i, j)
      count += startingHere
      val xHere = findX(i, j)
      if (xHere) {
        countX += 1
      }
      j += 1
    }
    i += 1
  }
  
  println(count)
  println(countX)
}