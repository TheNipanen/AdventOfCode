package day20

import scala.io.Source
import scala.collection.mutable.{HashMap, HashSet, Queue}

object RaceCondition extends App {
  val s = Source.fromFile("src/day20/input.txt")
  val grid = try s.getLines().toArray finally s.close()
  
  val yLen = grid.length
  val xLen = grid(0).length
  
  val startY = grid.indexWhere( _.contains("S") )
  val startX = grid(startY).indexOf('S')
  val endY = grid.indexWhere( _.contains("E") )
  val endX = grid(endY).indexOf('E')
  val st = (startY, startX)
  val en = (endY, endX)
  
  def manhattan(u: (Int, Int), v: (Int, Int)): Int = {
    math.abs(v._1 - u._1) + math.abs(v._2 - u._2)
  }
  
  def isTrack(u: (Int, Int)): Boolean = isTrack(u._1, u._2)
  def isTrack(y: Int, x: Int): Boolean = {
    y >= 0 && y < yLen && x >= 0 && x < xLen && Vector('.', 'S', 'E').contains(grid(y)(x))
  }
  
  def neighbors(u: (Int, Int)): Vector[(Int, Int)] = {
    Vector((u._1 + 1, u._2), (u._1 - 1, u._2), (u._1, u._2 + 1), (u._1, u._2 - 1)).filter( isTrack(_) )
  }
  
  def cheatNeighbors(u: (Int, Int), dist: Int): Vector[(Int, Int)] = {
    (for (y <- u._1 - dist to u._1 + dist; x <- u._2 - dist to u._2 + dist if manhattan(u, (y,x)) <= dist && isTrack(y,x)) yield (y, x)).toVector
  }
  
  def bfs(start: (Int, Int)): HashMap[(Int, Int), Int] = {
    val visited = HashSet[(Int, Int)]()
    val dist = HashMap[(Int, Int), Int]()
    
    val q = Queue[(Int, Int)]()
    
    visited(start) = true
    dist(start) = 0
    q.enqueue(start)
    
    while (q.nonEmpty) {
      val u = q.dequeue()
      for (v <- neighbors(u)) {
        if (!visited(v)) {
          visited(v) = true
          dist(v) = dist(u) + 1
          q.enqueue(v)
        }
      }
    }
    dist
  }
  
  val distFromStart = bfs(st)
  val distFromEnd = bfs(en)
  val noCheatDist = distFromStart(en)
  
  var savingCheats = 0
  var savingCheats20 = 0
  for (y <- 0 until yLen; x <- 0 until xLen if isTrack(y, x)) {
    val u = (y, x)
    for (v <- cheatNeighbors(u, 20)) {
      val dist = manhattan(u, v)
      val cheatedDist = distFromStart(u) + dist + distFromEnd(v)
      if (noCheatDist - cheatedDist >= 100) {
        savingCheats20 += 1
        if (dist == 2) savingCheats += 1
      }
    }
  }
  println(savingCheats)
  println(savingCheats20)
}