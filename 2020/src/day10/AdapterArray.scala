package day10

import scala.io.Source

object AdapterArray extends App {
  val s = Source.fromFile("src/day10/input.txt")
  val a = s.getLines().toArray.map( _.toInt ).sorted.zipWithIndex
  s.close()
  val l = a.length
  var i = 0
  val differences = scala.collection.mutable.HashMap[Int, Int]()
  def addDiff(cur: Int, next: Int) = {
    val d = next - cur
    if (d > 3) println("Difference larger than 3")
    if (d < 1) println("Difference less than 1")
    if (differences.contains(d)) differences(d) += 1
    else differences(d) = 1
  }
  var current = 0
  while (i < l) {
    val next = a(i)._1
    addDiff(current, next)
    current = next
    i += 1
  }
  val last = current + 3
  addDiff(current, last)
  println("1: " + differences(1))
  println("3: " + differences(3))
  println("Multiplied: " + (differences(1) * differences(3)))
  
//  def inner(i: Int): Long = {
//    //println(i + ":" + l)
//    //if (i == l) return 1L
//    val current = a(i)._1
//    val compatible = a.drop(i + 1).takeWhile( _._1 - current <= 3 )
//    //println(compatible.length)
//    if (compatible.length == 0) {
//      println("Final")
//      return 1L
//    }
//    return compatible.map( {x: (Int, Int) => inner(x._2)} ).sum
//  }
//  val count = {
//    val compatible = a.takeWhile( _._1 <= 3 )
//    compatible.map( {x: (Int, Int) => inner(x._2)} ).sum
//  }
  
  val aa = (a ++ Array((last, l)))
  val counts = new Array[Long](l+1)
  counts(l) = 1L
  i = l - 1
  while (i >= 0) {
    val cur = aa(i)._1
    val compatible = aa.drop(i + 1).takeWhile( _._1 - cur <= 3 )
    val count = compatible.map( {x: (Int, Int) => counts(x._2)} ).sum
    counts(i) = count
    i -= 1
  }
  val count = aa.takeWhile( _._1 <= 3 ).map( {x: (Int, Int) => counts(x._2)} ).sum
  println("Combinations: " + count)
}