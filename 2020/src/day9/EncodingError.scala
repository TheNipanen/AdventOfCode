package day9

import scala.io.Source

object EncodingError extends App {
  val s = Source.fromFile("src/day9/input.txt")
  val reader = s.bufferedReader()
  
  var line = reader.readLine()
  val q = scala.collection.mutable.Queue[Long]()
  
  def findPair(n: Long): Boolean = {
    var i = 0
    while (i < q.length) {
      val first = q(i)
      var j = i + 1
      while (j < q.length) {
        val second = q(j)
        if (first + second == n) return true
        j += 1
      }
      i += 1
    }
    return false
  }
  
  var invN = -1L
  while (line != null) {
    val l = q.length
    val n = line.toLong
    if (l == 25) {
      if (findPair(n)) {
        line = reader.readLine()
        q.dequeue()
        q.enqueue(n)
      } else {
        line = null
        invN = n
        println("Invalid number: " + n)
      }
    } else {
      q.enqueue(n)
      line = reader.readLine()
    }
  }
  
  val ss = Source.fromFile("src/day9/input.txt")
  val a = ss.getLines().toArray.map( _.toLong )
  a.foreach(println(_))
  val l = a.length
  var found = false
  var i = 0
  while (!found && i < l) {
    var j = i
    var sum = 0L
    val range = scala.collection.mutable.ArrayBuffer[Long]()
    while (!found && j < l && sum < invN) {
      val n = a(j)
      range += n
      sum += n
      if (sum == invN) {
        found = true
        println("Range: " + range)
        val (min, max) = (range.min, range.max)
        println("Min: " + min)
        println("Max: " + max)
        println("Sum: " + (min + max))
      }
      j += 1
    }
    i += 1
  }
}