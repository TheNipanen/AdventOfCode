package day16

import scala.io.Source
import scala.collection.mutable.{ ArrayBuffer, HashMap, HashSet }

object ProboscideaVolcanium extends App {
  val s = Source.fromFile("src/day16/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val valves = ArrayBuffer[String]()
  val edges = ArrayBuffer[(String, String)]()
  val dist = HashMap[String, HashMap[String, Int]]()
  val rate = HashMap[String, Int]()
  
  var i = 0
  while (i < lines.length) {
    val line = lines(i)
    val valve = line.drop(6).take(2)
    val r = line.drop(23).takeWhile( _.isDigit ).toInt
    val leads = line.drop(23).dropWhile( _.isDigit ).filter( _ != 's' ).drop(23).split(", ")
    
    valves += valve
    rate(valve) = r
    for (n <- leads) {
      val pair = (valve, n)
      edges += pair
    }
    i += 1
  }
  
  for (u <- valves) {
    dist(u) = HashMap()
    for (v <- valves) {
      if (u == v) dist(u)(v) = 0
      else if (edges.contains((u, v))) dist(u)(v) = 1
      else dist(u)(v) = 1000000
    }
  }
  for (k <- valves; i <- valves; j <- valves) {
    if (dist(i)(j) > dist(i)(k) + dist(k)(j)) {
      dist(i)(j) = dist(i)(k) + dist(k)(j)
    }
  }
  
  for ((u, d) <- dist) {
    if (u != "AA" && rate(u) == 0) {
      dist.remove(u)
    } else {
      for ((v, dd) <- d) {
        if (v != "AA" && rate(v) == 0) {
          d.remove(v)
        }
      }
    }
  }
  
  val visited = HashSet[String]()
  var maxFlow = 0L
  var timeLeft = 30
  var currentFlow = 0L
  val paths = ArrayBuffer[Vector[String]]()
  
  def visit(u: String): Unit = {
    if (timeLeft <= 0) {
      maxFlow = maxFlow max currentFlow
      return
    }
    visited(u) = true
    if (u != "AA") {
      timeLeft -= 1
    }
    val r = timeLeft * rate(u)
    currentFlow += r
    for ((v, d) <- dist(u)) {
      if (!visited(v)) {
        timeLeft -= d
        visit(v)
        timeLeft += d
      }
    }
    maxFlow = maxFlow max currentFlow
    currentFlow -= r
    if (u != "AA") {
      timeLeft += 1
    }
    visited(u) = false
  }
  val path = ArrayBuffer[String]()
  def generatePaths(u: String, timeLeft: Int): Unit = {
    visited(u) = true
    path += u
    paths += path.toVector
    for ((v, d) <- dist(u)) {
      if (!visited(v)) {
        val nextTime = timeLeft - d - 1
        if (nextTime > 1) {
          generatePaths(v, nextTime)
        }
      }
    }
    path -= u
    visited(u) = false
  }
  
  visit("AA")
  println("Most pressure that can be released: " + maxFlow)
  
  generatePaths("AA", 26)
  
  def flow(path: Vector[String]) = {
    //if (path.contains("AA")) println("WTF")
    var timeLeft = 26
    var current = "AA"
    var f = 0L
    for (ii <- 1 until path.length) {
      val next = path(ii)
      val d = dist(current)(next)
      timeLeft -= d
      timeLeft -= 1
      f += timeLeft * rate(next)
      current = next
    }
    f
  }
  
  val sorted = paths.toArray.map( p => { val f = flow(p); /*if (f != p._2) println("interesting" + f + "," + p._2);*/ (p, f) } )
  
  var x = 0
  maxFlow = 0L
  while (x < sorted.length) {
    var y = 0
    while (y < sorted.length) {
      val xPath = sorted(x)
      val yPath = sorted(y)
      if (x != y && xPath._2 + yPath._2 > maxFlow && xPath._1.forall( v => v == "AA" || !yPath._1.contains(v) ) && yPath._1.forall( v => v == "AA" || !xPath._1.contains(v) )) {
        maxFlow = maxFlow max (xPath._2 + yPath._2)
      }
      y += 1
    }
    x += 1
  }
  
  println("Most pressure that can be released as a pair: " + maxFlow)
}