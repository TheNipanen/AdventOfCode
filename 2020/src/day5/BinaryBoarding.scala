package day5

import scala.io.Source

object BinaryBoarding extends App {
  val s = Source.fromFile("src/day5/input.txt")
  val reader = s.bufferedReader()
  def bS(input: String, lowC: Char, highC: Char, start: Int, end: Int) = {
    var i = 0
    var len = input.length()
    var s = start
    var e = end
    while (i < len) {
      val mid = (s + e) / 2
      val c = input(i)
      if (c == lowC) e = mid
      else {
        require(c == highC)
        s = mid + 1
      }
      i += 1
    }
    require(s == e)
    s
  }
  def seatID(r: Int, c: Int) = r * 8 + c
  
  var line = reader.readLine()
  
  val rLowC = 'F'
  val rHighC = 'B'
  val cLowC = 'L'
  val cHighC = 'R'
  val splitPoint = 7
  val rEnd = 127
  val cEnd = 7
  val maxSeatID = seatID(rEnd, cEnd)
  
  var highest = 0
  
  val encountered = new Array[Boolean](maxSeatID)
  while (line != null) {
    val (rowInput, colInput) = line.splitAt(splitPoint)
    val r = bS(rowInput, rLowC, rHighC, 0, rEnd)
    val c = bS(colInput, cLowC, cHighC, 0, cEnd)
    val ID = seatID(r, c)
    encountered(ID) = true
    if (ID > highest) highest = ID
    line = reader.readLine()
  }
  
  println("Max: " + maxSeatID)
  println("Highest ID: " + highest)
  
  var i = 1
  while (i <= highest) {
    if (!encountered(i) && encountered(i - 1) && encountered(i + 1)) println("My seatID: " + i) // Should happen only once
    i += 1
  }
}