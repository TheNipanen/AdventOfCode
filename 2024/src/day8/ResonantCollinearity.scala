package day8

import scala.io.Source
import scala.collection.mutable.HashSet

object ResonantCollinearity extends App {
  val s = Source.fromFile("src/day8/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val antennas = lines.zipWithIndex.flatMap( line => line._1.zipWithIndex.map( ant => if (ant._1 != '.') (ant._1, line._2, ant._2) else null ) ).filter( _ != null )
  val antinodes = HashSet[(Int, Int)]()
  val antinodesAll = HashSet[(Int, Int)]()
  
  def isIn(i: Int, j: Int): Boolean = {
    i >= 0 && i < lines.length && j >= 0 && j < lines(0).length
  }
  
  val l = antennas.length
  var i = 0
  while (i < l) {
    var j = i + 1
    while (j < l) {
      val (a1, i1, j1) = antennas(i)
      val (a2, i2, j2) = antennas(j)
      if (a1 == a2) {
        val diffI = i2 - i1
        val diffJ = j2 - j1
        var k = 0
        var edgeFound = false
        while (!edgeFound) {
          val node1 = (i1 - k * diffI, j1 - k * diffJ)
          edgeFound = !isIn(node1._1, node1._2)
          if (!edgeFound) {
            antinodesAll(node1) = true
            if (k == 1) {
              antinodes(node1) = true
            }
          }
          k += 1
        }
        
        k = 0
        edgeFound = false
        while (!edgeFound) {
          val node2 = (i2 + k * diffI, j2 + k * diffJ)
          edgeFound = !isIn(node2._1, node2._2)
          if (!edgeFound) {
            antinodesAll(node2) = true
            if (k == 1) {
              antinodes(node2) = true
            }
          }
          k += 1
        }
      }
      j += 1
    }
    i += 1
  }
  
  println(antinodes.size)
  println(antinodesAll.size)
}