package day22

import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}

object MonkeyMarket extends App {
  val s = Source.fromFile("src/day22/input.txt")
  val secrets = try s.getLines().toArray.map( _.toLong ) finally s.close()
//  val secrets = Array[Long](1,2,3,2024)
  
  
  def mix(secret: Long, next: Long): Long = {
    secret ^ next
  }
  def prune(secret: Long): Long = {
    secret & 0xffffffL // mod 16777216
  }
  
  def next(secret: Long): Long = {
    val first = prune(mix(secret, secret << 6 /* mult 64 */))
    val second = prune(mix(first, first >>> 5 /* div 32 */))
    prune(mix(second, second << 11 /* mult 2048 */))
  }
  
  val currentSeq = Array.fill(secrets.length)((ArrayBuffer[Byte](), 0.toByte))
  val bestPrices = Array.fill(secrets.length)(HashMap[Vector[Byte], Byte]())
  val encounteredSeqs = HashSet[Vector[Byte]]()
  val seqs = ArrayBuffer[Vector[Byte]]()
  
  var count = 0
  while (count < 2000) {
    var i = 0
    while (i < secrets.length) {
      val nextSecret = next(secrets(i))
      secrets(i) = nextSecret
      val nextPrice = (nextSecret % 10).toByte
      val (prevSeq, prevPrice) = currentSeq(i)
      if (prevSeq.length == 4) {
        prevSeq.remove(0)
      }
      val diff = (nextPrice - prevPrice).toByte
      prevSeq += diff
      currentSeq(i) = (prevSeq, nextPrice.toByte)
      val vec = prevSeq.toVector
      if (!encounteredSeqs(vec)) seqs += vec
      encounteredSeqs(vec) = true
      if (!bestPrices(i).contains(vec)) {
        bestPrices(i)(vec) = nextPrice
      }
      i += 1
    }
    
    count += 1
  }
  
  println(secrets.sum)
  
  var best = 0L
  var j = 0
  while (j < seqs.length) {
    var sum = 0L
    val seq = seqs(j)
    var i = 0
    while (i < bestPrices.length) {
      val price = bestPrices(i).getOrElse(seq, 0.toByte).toLong
      sum += price
      i += 1
    }
    best = best max sum
    j += 1
  }
  println(best)
}