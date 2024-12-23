package day23

import scala.io.Source
import scala.collection.mutable.{HashMap, HashSet}

object LanParty extends App {
  val s = Source.fromFile("src/day23/input.txt")
  val connections = try s.getLines().toArray finally s.close()
//  val connections = Array("ka-co","ta-co","de-co","ta-ka","de-ta","ka-de")
  
  val computers = HashSet[String]()
  val neighbors = HashMap[String, Vector[String]]()
  
  def addNeighbor(c1: String, c2: String) = {
    if (neighbors.contains(c1)) {
      neighbors(c1) ++= Vector(c2)
    } else {
      neighbors(c1) = Vector(c2)
    }
  }
  
  for (conn <- connections) {
    val c1 = conn.takeWhile( _ != '-' )
    val c2 = conn.dropWhile( _ != '-' ).drop(1)
    computers(c1) = true
    computers(c2) = true
    addNeighbor(c1, c2)
    addNeighbor(c2, c1)
  }
  
  var sum = 0
  val computersArray = computers.toArray
  val l = computersArray.length
  var i = 0
  while (i < l) {
    var j = i + 1
    while (j < l) {
      var k = j + 1
      while (k < l) {
        val c1 = computersArray(i)
        val c2 = computersArray(j)
        val c3 = computersArray(k)
        if (
          neighbors(c1).contains(c2) && neighbors(c2).contains(c3) && neighbors(c3).contains(c1) &&
          (c1.startsWith("t") || c2.startsWith("t") || c3.startsWith("t"))
        ) {
          sum += 1
        }
        k += 1
      }
      j += 1
    }
    i += 1
  }
  println(sum)
  
  def findLargestGroup(computer: String): Vector[String] = {
    var maximum = Vector[String]()
    val set1 = neighbors(computer).toSet + computer
    for (n <- neighbors(computer)) {
      val set2 = neighbors(n).toSet + n
      val intersection = set1 & set2
      if (intersection.size > maximum.length) {
        maximum = intersection.toVector.sorted
      }
    }
    maximum
  }
  
  val groups = HashMap[Vector[String], Int]()
  computersArray.foreach( comp => {
    val group = findLargestGroup(comp)
    if (groups.contains(group)) {
      groups(group) += 1
    } else {
      groups(group) = 1
    }
  })
  
  val maxClique = groups.toVector.maxBy( _._2 )
  println(maxClique._1.mkString(","))
}