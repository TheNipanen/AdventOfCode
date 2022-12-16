package day15

import scala.io.Source
import scala.collection.mutable.{ ArrayBuffer, HashSet, HashMap, Stack }
import scala.math.abs

object BeaconExclusionZone extends App {
  val s = Source.fromFile("src/day15/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val beacons = HashSet[(Int, Int)]()
  val distance = HashMap[(Int, Int), Int]()
  val ranges = HashMap[Int, ArrayBuffer[(Int, Int)]]()
  
  def manhattanDistance(p1: (Int, Int), p2: (Int, Int)) = {
    abs(p2._1 - p1._1) + abs(p2._2 - p1._2)
  }
  
  def overlap(ranges: ArrayBuffer[(Int, Int)]) = {
    val sorted = ranges.sortBy( _._1 )
    val stack = Stack[(Int, Int)](sorted(0))
    
    for (j <- 1 until sorted.length) {
      val top = stack.top
      if (top._2 < sorted(j)._1 && top._2 != sorted(j)._1 - 1) {
        stack.push(sorted(j))
      } else if (top._2 < sorted(j)._2) {
        stack.pop()
        stack.push((top._1, sorted(j)._2))
      }
    }
    
    stack.toArray
  }
  
  var i = 0
  var minX = Int.MaxValue
  var maxX = Int.MinValue
  var maxDist = Int.MinValue
  while (i < lines.length) {
    var line = lines(i).drop(12)
    val sensorX = line.takeWhile( c => c.isDigit || c == '-' ).toInt
    line = line.dropWhile( _.isDigit ).drop(4)
    val sensorY = line.takeWhile( c => c.isDigit || c == '-' ).toInt
    line = line.dropWhile( _.isDigit ).drop(25)
    val beaconX = line.takeWhile( c => c.isDigit || c == '-' ).toInt
    line = line.dropWhile( _.isDigit ).drop(4)
    val beaconY = line.takeWhile( c => c.isDigit || c == '-' ).toInt
    
    minX = minX min sensorX min beaconX
    maxX = maxX max sensorX max beaconX
    
    val dist = manhattanDistance((sensorX, sensorY), (beaconX, beaconY))
    maxDist = maxDist max dist
    
    beacons((beaconX, beaconY)) = true
    distance((sensorX, sensorY)) = dist
    
    var range = (sensorY - dist, sensorY + dist)
    for (x <- sensorX to sensorX - dist by -1) {
      if (!ranges.contains(x)) {
        ranges(x) = ArrayBuffer()
      }
      ranges(x) += range
      range = (range._1 + 1, range._2 - 1)
    }
    range = (sensorY - dist + 1, sensorY + dist - 1)
    for (x <- sensorX + 1 to sensorX + dist) {
      if (!ranges.contains(x)) {
        ranges(x) = ArrayBuffer()
      }
      ranges(x) += range
      range = (range._1 + 1, range._2 - 1)
    }
    
    i += 1
  }
  
  var count = 0
  val y = 2000000
  for (x <- minX - maxDist to maxX + maxDist) {
    if (!beacons((x, y)) && distance.exists( pair => manhattanDistance((pair._1._1, pair._1._2), (x, y)) <= pair._2 )) {
      count += 1
    }
  }
  
  println("Positions that cannot contain a beacon in y=2000000: " + count)
  
  var continue = true
  var x = 0
  var yy = -1
  while (continue && x <= 4000000) {
    val r = ranges(x)
    val merged = overlap(r)
    if (merged.length > 1) {
      var j = 0
      while (continue && j < merged.length) {
        val range = merged(j)
        if (range._1 - 1 >= 0 && range._1 - 1 <= 4000000) {
          yy = range._1 - 1
          continue = false
        } else if (range._2 + 1 >= 0 && range._2 + 1 <= 4000000) {
          yy = range._2 + 1
          continue = false
        }
        j += 1
      }
    }
    if (continue) {
      x += 1
    }
  }
  
  println("x: " + x)
  println("y: " + yy)
  println("Tuning frequency: " + (x.toLong * 4000000L + yy.toLong))
}