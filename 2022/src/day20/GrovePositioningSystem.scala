package day20

import scala.io.Source
import scala.math.abs
import scala.collection.mutable.ArrayBuffer


object GrovePositioningSystem extends App {
  val s = Source.fromFile("src/day20/input.txt")
  val initialSeq = try s.getLines().toArray.map( _.toInt ) finally s.close()
  
  var currentSeq = initialSeq.clone().map( _.toLong ).zipWithIndex
  val zeroIdx = currentSeq.find( _._1 == 0 ).get._2
  
  def move(i: Int) = {
    currentSeq = rotate(-i.toLong)
    val (v, ii) = currentSeq(0)
    currentSeq = currentSeq.tail
    currentSeq = rotate(-v)
    currentSeq = (v, ii) +: currentSeq
  }
  
  def mapInd(i: Int) = {
    i % currentSeq.length
  }
  
  def mix() = {
    for (i <- 0 until initialSeq.length) {
      val (v, _) = currentSeq.find( _._2 == i ).get
      val ii = currentSeq.indexOf( (v, i) )
      move(ii)
    }
  }
  def rotate(amount: Long) = {
    if (amount < 0L) {
      rotateLeft(-amount)
    } else {
      rotateRight(amount)
    }
  }
  def rotateLeft(amount: Long) = {
    val size = currentSeq.size.toLong
    val (first, last) = currentSeq.splitAt((amount % size).toInt)
    last ++ first
  }
  def rotateRight(amount: Long) = {
    val size = currentSeq.size.toLong
    val (first, last) = currentSeq.splitAt((size - (amount % size)).toInt)
    last ++ first
  }
  
  mix()
  
  val idx = currentSeq.indexOf( (0L, zeroIdx) )
  val sum = currentSeq(mapInd(idx + 1000).toInt)._1 + currentSeq(mapInd(idx + 2000).toInt)._1 + currentSeq(mapInd(idx + 3000).toInt)._1
    
  println("Sum of the grove coordinates: " + sum)
  
  currentSeq = initialSeq.clone().map( _.toLong * 811589153L ).zipWithIndex
  var mixes = 0
  while (mixes < 10) {
    mix()
    mixes += 1
  }
  
  val idx2 = currentSeq.indexOf( (0L, zeroIdx) )
  val sum2 = currentSeq(mapInd(idx2 + 1000).toInt)._1 + currentSeq(mapInd(idx2 + 2000).toInt)._1 + currentSeq(mapInd(idx2 + 3000).toInt)._1
  
  println("Sum of the grove coordinates with decryption key: " + sum2)
}