package day18

import scala.io.Source
import scala.collection.mutable.{HashMap, HashSet, Queue}

object RamRun extends App {
  val s = Source.fromFile("src/day18/input.txt")
  val bytes = try s.getLines().toArray finally s.close()
  
  val yLen = 71
  val xLen = 71
  val grid = Array.fill(yLen, xLen)('.')
  
  def dropByte(i: Int) = {
    val byte = bytes(i).split(",")
    val x = byte(0).toInt
    val y = byte(1).toInt
    grid(y)(x) = '#'
  }
  
  for (i <- 0 until 1024) {
    dropByte(i)
  }
  
  def neighbors(u: (Int, Int)): Vector[(Int, Int)] = {
    Vector((u._1 + 1, u._2), (u._1 - 1, u._2), (u._1, u._2 + 1), (u._1, u._2 - 1))
      .filter( pair => pair._1 >= 0 && pair._1 < yLen && pair._2 >= 0 && pair._2 < xLen && grid(pair._1)(pair._2) == '.' )
  }
  
  def bfs(): HashMap[(Int, Int), Int] = {
    val visited = HashSet[(Int, Int)]()
    val dist = HashMap[(Int, Int), Int]()
    
    val q = Queue[(Int, Int)]()
    
    val start = (0,0)
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
  
  
  
  println(bfs()((70, 70)))
  
  var i = 1024
  var found = false
  while (!found && i < bytes.length) {
    dropByte(i)
    val dist = bfs()
    if (dist.contains((70, 70))) {
      i += 1
    } else {
      found = true
    }
  }
  
  println(bytes(i))
}