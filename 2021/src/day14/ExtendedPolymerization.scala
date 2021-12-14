package day14

import scala.io.Source
import scala.collection.mutable.HashMap

object ExtendedPolymerization extends App {
  val s = Source.fromFile("src/day14/input.txt")
  val lines = s.getLines().toArray
  s.close()
  var current = lines(0)
  val rules = HashMap[String,Char]()
  for (i <- 2 until lines.size) {
    val ss = lines(i).split(" -> ")
    rules(ss(0)) = ss(1)(0)
  }
  
  def iterate() = {
    var next = ""
    next += current(0)
    var j = 1
    while (j < current.size) {
      val first = current(j-1)
      val second = current(j)
      val middle = rules(first.toString + second)
      next += middle
      next += second
      j += 1
    }
    current = next
    steps += 1
  }
  
  val count = HashMap[Char, Int]()
  var steps = 0
  while (steps < 10) {
    iterate()
  }
  
  var maxC = 0
  var minC = Int.MaxValue
  for (c <- current) {
    if (count.get(c).isDefined) count(c) = count(c) + 1
    else count(c) = 1
  }
  for (p <- count) {
    maxC = p._2 max maxC
    minC = p._2 min minC
  }
  val difference = maxC - minC
  println("Difference between the quantities of the most common and least common elements: " + difference)
  
  var countSL = HashMap[String,Long]()
  for (i <- 0 until current.size - 1) {
    val str = current.drop(i).take(2)
    if (countSL.get(str).isEmpty) countSL(str) = 1
    else countSL(str) += 1
  }
  while (steps < 40) {
    val nextCountSL = HashMap[String,Long]()
    for (p <- countSL) {
      val c = rules(p._1)
      val first = p._1.take(1) + c
      val second = c + p._1.drop(1)
      if (nextCountSL.get(first).isEmpty) nextCountSL(first) = p._2
      else nextCountSL(first) += p._2
      if (nextCountSL.get(second).isEmpty) nextCountSL(second) = p._2
      else nextCountSL(second) += p._2
    }
    countSL = nextCountSL
    steps += 1
  }
  val countL = HashMap[Char,Long]()
  for (p <- countSL) {
    val first = p._1(0)
    if (countL.get(first).isEmpty) countL(first) = p._2
    else countL(first) += p._2
  }
  countL(current(current.size-1)) += 1
  
  var maxL = 0L
  var minL = Long.MaxValue
  for (p <- countL) {
    maxL = p._2 max maxL
    minL = p._2 min minL
  }
  val differenceL = maxL - minL
  println("Difference between the quantities of the most common and least common elements: " + differenceL)
}
