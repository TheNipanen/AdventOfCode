package day3

import scala.io.Source

object TobogganTrajectory extends App {
  val s = Source.fromFile("src/day3/input.txt")
  val reader = s.bufferedReader()
  var trees1 = 0L
  var trees2 = 0L
  var trees3 = 0L
  var trees4 = 0L
  var trees5 = 0L
  
  //Start from the left
  var i1 = 0
  var i2 = 0
  var i3 = 0
  var i4 = 0
  var i5 = 0
  var iLen = 0
  var line = reader.readLine()
  var len = line.length()
  println(len)
  while (line != null) {
    val c1 = line(i1)
    val c2 = line(i2)
    val c3 = line(i3)
    val c4 = line(i4)
    val c5 = line(i5)
    
    if (c1 == '#') trees1 += 1
    if (c2 == '#') trees2 += 1
    if (c3 == '#') trees3 += 1
    if (c4 == '#') trees4 += 1
    if (iLen % 2 == 0 && c5 == '#') trees5 += 1
    
    i1 = (i1 + 1) % len
    i2 = (i2 + 3) % len
    i3 = (i3 + 5) % len
    i4 = (i4 + 7) % len
    if (iLen % 2 == 0) i5 = (i5 + 1) % len
    iLen += 1
    line = reader.readLine()
  }
  println(iLen)
  println("Trees1: " + trees1)
  println("Trees2: " + trees2)
  println("Trees3: " + trees3)
  println("Trees4: " + trees4)
  println("Trees5: " + trees5)
  println("Multiplied: " + (trees1 * trees2 * trees3 * trees4 * trees5))
}