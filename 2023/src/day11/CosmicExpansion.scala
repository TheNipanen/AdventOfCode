package day11

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object CosmicExpansion extends App {
  val s = Source.fromFile("src/day11/input.txt")
  val l = try s.getLines().toArray finally s.close()
  var lines = l.clone()
  
  var i = 0
  while (i < lines.length) {
    val line = lines(i)
    if (line.forall( _ == '.' )) {
      lines = lines.take(i) ++ Array(line, line) ++ lines.drop(i + 1)
      i += 1
    }
    i += 1
  }
  
  i = 0
  while (i < lines(0).length) {
    if (lines.forall( _(i) == '.' )) {
      lines = lines.map( line => line.take(i) + ".." + line.drop(i + 1) )
      i += 1
    }
    i += 1
  }
  
  val g = ArrayBuffer[(Int, Int)]()
  i = 0
  while (i < lines.length) {
    val line = lines(i)
    var j = 0
    while (j < line.length) {
      if (line(j) == '#') {
        g += ((i, j))
      }
      j += 1
    }
    i += 1
  }
  
  val galaxies = g.toArray
  val pairs = galaxies.combinations(2)
  
  def manhattan(c1: (Int, Int), c2: (Int, Int)): Int = {
    math.abs(c1._1 - c2._1) + math.abs(c1._2 - c2._2)
  }
  val sum = pairs.map( pair => manhattan(pair(0), pair(1)) ).sum
  println("Sum of pairwise shortest paths: " + sum)
  
  lines = l.clone()
  i = 0
  while (i < lines.length) {
    val line = lines(i)
    if (line.forall( _ == '.' )) {
      lines = lines.take(i) ++ Array("," * line.length) ++ lines.drop(i + 1)
    }
    i += 1
  }
  
  i = 0
  while (i < lines(0).length) {
    if (lines.forall( line => line(i) == '.' || line(i) == ',' )) {
      lines = lines.map( line => line.take(i) + "," + line.drop(i + 1) )
    }
    i += 1
  }
  
  val g2 = ArrayBuffer[(Long, Long)]()
  var ii = 0L
  while (ii < lines.length) {
    val line = lines(ii.toInt)
    var jj = 0L
    while (jj < line.length) {
      if (line(jj.toInt) == '#') {
        g2 += ((ii, jj))
      }
      jj += 1L
    }
    ii += 1L
  }
  
  val galaxies2 = g2.toArray
  val pairs2 = galaxies2.combinations(2)
  
  def manhattan(c1: (Long, Long), c2: (Long, Long)): Long = {
    val verticalCount = lines.map( _(0) ).slice((c1._1 min c2._1).toInt, (c1._1 max c2._1).toInt + 1).count( _ == ',' ).toLong
    val horizontalCount = lines(0).slice((c1._2 min c2._2).toInt, (c1._2 max c2._2).toInt + 1).count( _ == ',' ).toLong
    val res = (math.abs(c1._1 - c2._1) - verticalCount
    + verticalCount * 1000000L
    + math.abs(c1._2 - c2._2) - horizontalCount
    + horizontalCount * 1000000L)
    res
  }
  val sum2 = pairs2.map( pair => manhattan(pair(0), pair(1)) ).sum
  println("Sum of pairwise shortest paths in big universe: " + sum2)
}