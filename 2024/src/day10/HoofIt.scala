package day10

import scala.io.Source
import scala.collection.mutable.{HashSet, Queue}

object HoofIt extends App {
  val s = Source.fromFile("src/day10/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val yLen = lines.length
  val xLen = lines(0).length
  
  def neighbors(u: (Int, Int)): Array[(Int, Int)] = {
    val neigh = Array((u._1 - 1, u._2), (u._1, u._2 + 1), (u._1 + 1, u._2), (u._1, u._2 - 1))
    val d = lines(u._1)(u._2).asDigit
    neigh.filter( v => v._1 >= 0 && v._1 < yLen && v._2 >= 0 && v._2 < xLen && lines(v._1)(v._2).asDigit == d + 1)
  }
  
  def getScore(y: Int, x: Int, singlePath: Boolean): Int = {
    val visited = HashSet[(Int, Int)]()
    val q = Queue[(Int, Int)]()
    
    visited((y, x)) = true
    q.enqueue((y, x))
    
    var score = 0
    
    while (q.nonEmpty) {
      val u = q.dequeue()
      if (lines(u._1)(u._2) == '9') {
        score += 1
      }
      for (v <- neighbors(u)) {
        if (!visited(v) || !singlePath) {
          visited(v) = true
          q.enqueue(v)
        }
      }
    }
    
    score
  }
  
  var scores = 0
  var ratings = 0
  var y = 0
  while (y < yLen) {
    var x = 0
    while (x < xLen) {
      val c = lines(y)(x)
      if (c == '0') {
        val score = getScore(y, x, true)
        var rating = getScore(y, x, false)
        scores += score
        ratings += rating
      }
      x += 1
    }
    y += 1
  }
  
  println(scores)
  println(ratings)
}