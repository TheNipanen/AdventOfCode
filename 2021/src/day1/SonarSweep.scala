package day1

import scala.io.Source

object SonarSweep extends App {
  val s = Source.fromFile("src/day1/input.txt")
  val reader = s.bufferedReader()
  
  var line = reader.readLine()
  var increaseCount = 0
  while (line != null) {
    val nextLine = reader.readLine()
    if (nextLine != null && line.toInt < nextLine.toInt) {
      increaseCount += 1
    }
    line = nextLine
  }
  s.close()
  println("Measurement increases: " + increaseCount)
  
  val ss = Source.fromFile("src/day1/input.txt")
  val measurements = ss.getLines().toArray.map( _.toInt )
  ss.close()
  var i = 3
  var prevSum = measurements(0) + measurements(1) + measurements(2)
  increaseCount = 0
  while (i < measurements.size) {
    val nextSum = prevSum - measurements(i-3) + measurements(i)
    if (prevSum < nextSum) {
      increaseCount += 1
    }
    prevSum = nextSum
    i += 1
  }
  println("Three-measeurement increases: " + increaseCount)
}