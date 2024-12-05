package day5

import scala.io.Source
import scala.collection.mutable.{HashMap, HashSet, Queue}

object PrintQueue extends App {
  val s = Source.fromFile("src/day5/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val rules = lines.takeWhile( _ != "" )
  val updates = lines.dropWhile( _ != "" ).drop(1)
  
  val predecessors = HashMap[String, HashSet[String]]()
  for (rule <- rules) {
    val first = rule.takeWhile( _ != '|' )
    val second = rule.dropWhile( _ != '|' ).drop(1)
    if (predecessors.contains(second)) {
      predecessors(second).add(first)
    } else {
      predecessors(second) = HashSet(first)
    }
  }
  
  def shouldComeBefore(first: String, second: String): Boolean = {
    predecessors.getOrElse(first, HashSet())(second)
  }
  
  var sumOfCorrectMiddlePages = 0
  var sumOfWrongMiddlePages = 0
  val l = updates.length
  var i = 0
  while (i < l) {
    val update = updates(i).split(",")
    val uLength = update.length
    var isRight = true
    var j = 0
    while (isRight && j < uLength) {
      var k = j + 1
      while (isRight && k < uLength) {
        if (shouldComeBefore(update(j), update(k))) {
          isRight = false
        }
        k += 1
      }
      j += 1
    }
    if (isRight) {
      sumOfCorrectMiddlePages += update(uLength / 2).toInt
    } else {
      val sorted = update.sortWith((first: String, second: String) => !shouldComeBefore(first, second))
      sumOfWrongMiddlePages += sorted(uLength / 2).toInt
    }
    i += 1
  }
  
  println(sumOfCorrectMiddlePages)
  println(sumOfWrongMiddlePages)
}