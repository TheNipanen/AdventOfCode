package day12

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

object PassagePathing extends App {
  val s = Source.fromFile("src/day12/input.txt")
  val edges = s.getLines().toArray.map( _.split("-") )
  s.close()
  val neighbors = HashMap[String,ArrayBuffer[String]]()
  for (e <- edges) {
    val v = e(0)
    val u = e(1)
    val nV = neighbors.get(v)
    val nU = neighbors.get(u)
    if (nV.isDefined) {
      neighbors(v) += u
    } else {
      neighbors(v) = ArrayBuffer(u)
    }
    if (nU.isDefined) {
      neighbors(u) += v
    } else {
      neighbors(u) = ArrayBuffer(v)
    }
  }
  
  var paths = 0
  val visited = HashSet[String]()
  def visit(u: String): Unit = {
    visited(u) = true
    for (n <- neighbors(u)) {
      val end = n == "end"
      if (end || !n(0).isLower || !visited(n)) {
        if (end) {
          paths += 1
        } else {
          visit(n)
        }
      }
    }
    visited(u) = false
  }
  
  visit("start")
  println("Unique paths: " + paths)
  
  var paths2 = 0L
  val visited2 = HashMap[String,Int]()
  var twiceVisited: String = ""
  def visit2(u: String): Unit = {
    if (visited2.get(u).isEmpty) visited2(u) = 0
    visited2(u) += 1
    for (n <- neighbors(u)) {
      val end = n == "end"
      val start = n == "start"
      if (!start && (end || !n(0).isLower || visited2.get(n).isEmpty || visited2(n) < 2)) {
        if (end) {
          paths2 += 1
        } else {
          if (n(0).isLower && visited2.get(n).isDefined && visited2(n) == 1) {
            if (twiceVisited == "") {
              twiceVisited = n
              visit2(n)
              twiceVisited = ""
            }
          } else {
            visit2(n)
          }
        }
      }
    }
    visited2(u) -= 1
  }
  
  visit2("start")
  println("Unique paths now: " + paths2)
}