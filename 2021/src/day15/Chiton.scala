package day15

import scala.io.Source

object Chiton extends App {
  val s = Source.fromFile("src/day15/input.txt")
  var origGrid = s.getLines().toArray.map(_.split("").map(_.toInt))
  s.close()
  var grid = origGrid.clone.map(_.clone)
  var rows = grid.size
  var cols = grid(0).size

  def neighbors(r: Int, c: Int) = {
    Array((r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)).filter(pair => pair._1 >= 0 && pair._1 < rows && pair._2 >= 0 && pair._2 < cols)
  }

  def dijkstra() = {
    val q = scala.collection.mutable.PriorityQueue[(Int, (Int, Int))]()(Ordering.by(-_._1))
    val pred = Array.fill[(Int, Int)](rows, cols)(-1, -1)
    val processed = Array.ofDim[Boolean](rows, cols)
    val dist = Array.fill[Int](rows, cols)(Int.MaxValue)

    q.enqueue(0 -> (0, 0))
    while (q.nonEmpty) {
      val uu = q.dequeue

      for (v <- neighbors(uu._2._1, uu._2._2)) {
        val w = grid(v._1)(v._2)
        if (!processed(v._1)(v._2) || dist(uu._2._1)(uu._2._2) + w < dist(v._1)(v._2)) {
          processed(v._1)(v._2) = true
          pred(v._1)(v._2) = uu._2
          dist(v._1)(v._2) = dist(uu._2._1)(uu._2._2) + w
          q.enqueue(dist(v._1)(v._2) -> v)
        }
      }
    }

    var i = rows - 1
    var j = cols - 1
    val res = collection.mutable.ArrayBuffer[(Int, Int)](((i, j)))
    while (((i, j)) != (0, 0)) {
      val par = pred(i)(j)
      res += par
      i = par._1
      j = par._2
    }

    val lowestRisk = res.reverse.drop(1).map(pair => grid(pair._1)(pair._2)).sum
    println("Lowest risk: " + lowestRisk)
  }
  
  dijkstra()

  for (ii <- 0 until 4) {
    for (r <- 0 until rows) {
      origGrid(r) = origGrid(r).map( i => if (i == 9) 1 else i + 1 )
      grid(r) = grid(r) ++ origGrid(r)
    }
  }
  origGrid = grid.clone.map(_.clone)
  for (ii <- 0 until 4) {
    origGrid = origGrid.map( _.map( i => if (i == 9) 1 else i + 1 ) )
    grid = grid ++ origGrid
  }
  rows = grid.size
  cols = grid(0).size
  
  dijkstra()
}