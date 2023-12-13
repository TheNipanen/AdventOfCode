package day9

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object MirageMaintenance extends App {
  val s = Source.fromFile("src/day9/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  var sum = 0L
  var sum2 = 0L
  var i = 0
  val l = lines.length
  while (i < l) {
    val line = lines(i)
    val sequences = ArrayBuffer[ArrayBuffer[Long]](ArrayBuffer())
    sequences(0) ++= line.split(" ").map( _.toLong )
    
    var j = 0
    while (sequences(j).exists( _ != 0L )) {
      val seq = sequences(j)
      val next = ArrayBuffer[Long]()
      var k = 1
      while (k < seq.length) {
        next += seq(k) - seq(k - 1)
        k += 1
      }
      sequences += next
      j += 1
    }
    
    sequences(j) += 0L
    sequences(j).prepend(0L)
    j -= 1
    while (j >= 0) {
      val currentSeq = sequences(j)
      val last = currentSeq(currentSeq.length - 1)
      val prevSeq = sequences(j + 1)
      val prevLast = prevSeq(prevSeq.length - 1)
      val next = last + prevLast
      currentSeq += next
      
      val first = currentSeq(0)
      val prevFirst = prevSeq(0)
      val nextBeginning = first - prevFirst
      currentSeq.prepend(nextBeginning)
      j -= 1
    }
    
    sum += sequences(0)(sequences(0).length - 1)
    sum2 += sequences(0)(0)
    
    i += 1
  }
  
  println("Sum of extrapolated values: " + sum)
  println("Sum of beginning extrapolated values: " + sum2)
}