package day16

import scala.io.Source
import scala.collection.mutable.{HashMap, HashSet, PriorityQueue}

object ReindeerMaze extends App {
  val s = Source.fromFile("src/day16/input.txt")
  val grid = try s.getLines().toArray finally s.close()
  
  val endY = grid.indexWhere( _.contains('E') )
  val endX = grid(endY).indexOf('E')
  val startY = grid.indexWhere( _.contains('S') )
  val startX = grid(startY).indexOf('S')
  
  def neighbors(u: (Int, Int, Int, Int), backwards: Boolean): Vector[(Int, Int, Int, Int)] = {
    val nextY = if (backwards) u._1 - u._3 else u._1 + u._3
    val nextX = if (backwards) u._2 - u._4 else u._2 + u._4
    val turnCWY = u._4
    val turnCWX = - u._3
    val turnCCWY = - u._4
    val turnCCWX = u._3
    val turns = Vector((u._1, u._2, turnCWY, turnCWX), (u._1, u._2, turnCCWY, turnCCWX))
    if (grid(nextY)(nextX) == '#') turns
    else turns ++ Vector((nextY, nextX, u._3, u._4))
  }
  
  def dijkstra(start: (Int, Int, Int, Int), backwards: Boolean) = {
    val visited = HashSet[(Int, Int, Int, Int)]()
    val dist = HashMap[(Int, Int, Int, Int), Long]()
    
    val q = PriorityQueue[((Int, Int, Int, Int), Long)]()(Ordering.by(-_._2))
    visited(start) = true
    dist(start) = 0L
    q.enqueue(start -> 0L)
    
    while (q.nonEmpty) {
      val (u, d) = q.dequeue()
      for (v <- neighbors(u, backwards)) {
        val weight = if (v._1 == u._1 && v._2 == u._2) 1000L else 1L
        if (!visited(v) || dist(u) + weight < dist(v)) {
          visited(v) = true
          dist(v) = dist(u) + weight
          q.enqueue(v -> dist(v))
        }
      }
    }
    
    dist
  }
  
  val start = (startY, startX, 0, 1)
  val distFromStart = dijkstra(start, false)
  
  val directions = Vector((1,0), (-1,0), (0, 1), (0, -1))
  
  def getLowest(distances: HashMap[(Int, Int, Int, Int), Long]): Long = {
    directions.map( dir => (endY, endX, dir._1, dir._2) ).map( distances(_) ).min
  }
  
  val lowestScore = getLowest(distFromStart)
  println(lowestScore)
  
  val distsFromEnd = directions.map( dir => (endY, endX, dir._1, dir._2) ).map( dijkstra(_, true) )
  
  var bestTiles = 0
  for (y <- 0 until grid.length; x <- 0 until grid(y).length; if grid(y)(x) != '#') {
    if (directions.exists( dir => {
      val lowestToHere = distFromStart((y, x, dir._1, dir._2))
      val lowestFromHere = distsFromEnd.map( _((y, x, dir._1, dir._2)) ).min
      lowestToHere + lowestFromHere == lowestScore
    } )) {
      bestTiles += 1
    }
  }
  println(bestTiles)
}