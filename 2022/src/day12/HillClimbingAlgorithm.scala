package day12

import scala.io.Source
import scala.collection.mutable.{ ArrayBuffer, HashSet, PriorityQueue }

object HillClimbingAlgorithm extends App {
  val s = Source.fromFile("src/day12/input.txt")
  val lines = try s.getLines().toArray.map( _.toCharArray() ) finally s.close()

  val width = lines(0).length
  val height = lines.length

  def dijkstra(): Option[ArrayBuffer[(Int, Int)]] = {
    val startLine = lines.find(_.contains('S')).get
    val start = (startLine.indexOf('S'), lines.indexOf(startLine))
    val goalLine = lines.find(_.contains('E')).get
    val goal = (goalLine.indexOf('E'), lines.indexOf(goalLine))

    val dist = Array.fill(width, height)(Int.MaxValue)
    val pred: Array[Array[Option[(Int, Int)]]] = Array.fill(width, height)(None)
    dist(start._1)(start._2) = 0

    val S = HashSet[(Int, Int)]()
    val Q = PriorityQueue[((Int, Int), Int)]((start, 0))(Ordering.by(-_._2))

    def getChar(u: (Int, Int)) = {
      val uu = lines(u._2)(u._1)
      if (uu == 'S') 'a'
      else if (uu == 'E') 'z'
      else uu
    }

    def diff(u: (Int, Int), v: (Int, Int)) = {
      val uu = getChar(u)
      val vv = getChar(v)
      vv - uu
    }

    def neighbors(u: (Int, Int)) = {
      Array((u._1 + 1, u._2), (u._1 - 1, u._2), (u._1, u._2 + 1), (u._1, u._2 - 1))
        .filter(v => v._1 >= 0 && v._1 < width && v._2 >= 0 && v._2 < height && diff(u, v) <= 1)
    }

    while (Q.nonEmpty) {
      val (u, d) = Q.dequeue()
      if (!S(u)) {
        S(u) = true
        for (v <- neighbors(u)) {
          val d = dist(u._1)(u._2) + 1
          if (d < dist(v._1)(v._2)) {
            dist(v._1)(v._2) = d
            pred(v._1)(v._2) = Some(u)
            Q.enqueue((v, d))
          }
        }
      }
    }

    if (pred(goal._1)(goal._2) == None) {
      return None
    }
    
    var current = goal
    val path = ArrayBuffer[(Int, Int)]()
    while (pred(current._1)(current._2).isDefined) {
      path += current
      current = pred(current._1)(current._2).get
    }
    Some(path)
  }

  val path = dijkstra()
  println("Fewest steps to goal: " + path.get.length)
  
  val origStartLine = lines.find(_.contains('S')).get
  val origStart = (origStartLine.indexOf('S'), lines.indexOf(origStartLine))
  lines(origStart._2)(origStart._1) = 'a'
  
  var minSteps = Int.MaxValue
  for (i <- 0 until height; j <- 0 until width) {
    if (lines(i)(j) == 'a') {
      lines(i)(j) = 'S'
      val path = dijkstra()
      if (path.isDefined) {
        minSteps = minSteps min path.get.length
      }      
      lines(i)(j) = 'a'
    }
  }
  
  println("Fewest steps to goal from any square with elevation a: " + minSteps)
}