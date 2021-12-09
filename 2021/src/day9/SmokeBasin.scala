package day9

import scala.io.Source

object SmokeBasin extends App {
  val s = Source.fromFile("src/day9/input.txt")
  val grid = s.getLines().toArray.map( _.split("").map(_.toInt) )
  s.close
  
  val rows = grid.size
  val columns = grid(0).size
  var sum = 0
  var row = 0
  val lowPoints = scala.collection.mutable.ArrayBuffer[(Int,Int)]()
  while (row < rows) {
    var col = 0
    while (col < columns) {
      val point = grid(row)(col)
      if ((row == 0 || grid(row-1)(col) > point)
          && (row == rows-1 || grid(row+1)(col) > point)
          && (col == 0 || grid(row)(col-1) > point)
          && (col == columns-1 || grid(row)(col+1) > point)
      ) {
        val riskLevel = 1 + point
        sum += riskLevel
        lowPoints += ((row,col))
      }
      col += 1
    }
    row += 1
  }
  println("Sum of low point risk levels: " + sum)
  
  def neighbors(u: (Int,Int)) = {
    val neigh = Array((u._1-1,u._2),(u._1+1,u._2),(u._1,u._2-1),(u._1,u._2+1))
    neigh.filter( pair => pair._1 >= 0 && pair._1 < rows && pair._2 >= 0 && pair._2 < columns )
  }
  
  val visited = Array.fill[Boolean](rows,columns)(false)
  val sizes = scala.collection.mutable.PriorityQueue[Int]()
  
  def bfs(source: (Int,Int)) = {
    val q = scala.collection.mutable.Queue[(Int,Int)]()
    visited(source._1)(source._2) = true
    var size = 1
    q.enqueue(source)
    while (q.nonEmpty) {
      val u = q.dequeue()
      for (v <- neighbors(u)) {
        if (!visited(v._1)(v._2) && grid(v._1)(v._2) != 9) {
          size += 1
          visited(v._1)(v._2) = true
          q.enqueue(v)
        }
      }
    }
    sizes.enqueue(size)
  }
  
  lowPoints.foreach(bfs(_))
  val mult = sizes.dequeue() * sizes.dequeue() * sizes.dequeue()
  println("Three largest basins multiplied: " + mult)
}