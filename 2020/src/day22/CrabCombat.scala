package day22

import scala.io.Source
import scala.collection.mutable.Queue

object CrabCombat extends App {
  val s = Source.fromFile("src/day22/input.txt")
  val r = s.bufferedReader()
  var line = r.readLine()
  line = r.readLine()
  val p1 = Queue[Int]()
  val p2 = Queue[Int]()
  while (line != "") {
    p1.enqueue(line.toInt)
    line = r.readLine()
  }
  
  r.readLine()
  line = r.readLine()
  while (line != null) {
    p2.enqueue(line.toInt)
    line = r.readLine()
  }
  val startP1 = p1.clone()
  val startP2 = p2.clone()
  
  while (p1.nonEmpty && p2.nonEmpty) {
    val p1C = p1.dequeue()
    val p2C = p2.dequeue()
    if (p1C > p2C) {
      p1.enqueue(p1C, p2C)
    } else {
      p2.enqueue(p2C, p1C)
    }
  }
  
  val winner = if (p1.nonEmpty) 1 else 2
  var sum = 0
  var i = if (p1.nonEmpty) p1.length else p2.length
  while (i > 0) {
    val c = if (p1.nonEmpty) p1.dequeue() else p2.dequeue()
    sum += i * c
    i -= 1
  }
  
  println("Winner: " + winner + ", Winning score: " + sum)
  
  def game(p1D: Queue[Int], p2D: Queue[Int]): Int = {
    val prev = scala.collection.mutable.ArrayBuffer[(Array[Int], Array[Int])]()
    def samePos: Boolean = {
      var i = 0
      while (i < prev.length) {
        val (p1P, p2P) = prev(i)
        if (p1D.length == p1P.length && p2D.length == p2P.length) {
          var eq = true
          var j = 0
          while (j < p1D.length) {
            if (p1D(j) != p1P(j)) eq = false
            j += 1
          }
          j = 0
          while (j < p2D.length) {
            if (p2D(j) != p2P(j)) eq = false
            j += 1
          }
          if (eq) return true
        }
        i += 1
      }
      false
    }
    
    while (p1D.nonEmpty && p2D.nonEmpty) {
      if (samePos) return 1
      prev += ((p1D.toArray, p2D.toArray))
      val p1C = p1D.dequeue()
      val p2C = p2D.dequeue()
      var winner = -1
      if (p1D.length >= p1C && p2D.length >= p2C) {
        winner = game(p1D.take(p1C), p2D.take(p2C))
      } else {
        if (p1C > p2C) winner = 1 else winner = 2
      }
      if (winner == 1) p1D.enqueue(p1C, p2C)
      else p2D.enqueue(p2C, p1C)
    }
    if (p1D.nonEmpty) 1
    else 2
  }
  
  val winner2 = game(startP1, startP2)
  sum = 0
  i = if (winner2 == 1) startP1.length else startP2.length
  while (i > 0) {
    val c = if (winner2 == 1) startP1.dequeue() else startP2.dequeue()
    sum += i * c
    i -= 1
  }
  
  println("Winner of recursive combat: " + winner2 + ", Winning score: " + sum)
}