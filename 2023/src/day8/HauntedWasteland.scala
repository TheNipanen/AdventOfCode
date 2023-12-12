package day8

import scala.io.Source
import scala.collection.mutable.HashMap

object HauntedWasteland extends App {
  val s = Source.fromFile("src/day8/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val instructions = lines(0)
  val next = HashMap[String, (String, String)]()
  
  var i = 2
  val l = lines.length
  while (i < l) {
    val line = lines(i)
    val start = line.substring(0, 3)
    val left = line.substring(7, 10)
    val right = line.substring(12, 15)
    next(start) = (left, right)
    i += 1
  }
  
  i = 0
  
  def nextNode(current: String): String = {
    val ins = instructions(i)
    ins match { case 'L' => next(current)._1 case 'R' => next(current)._2 }
  }
  
  var steps = 0L
  var current = "AAA"
  while (current != "ZZZ") {
    current = nextNode(current)
    steps += 1L
    i += 1
    if (i == instructions.length) {
      i = 0
    }
  }
  
  println("Steps to reach ZZZ: " + steps)
  
  i = 0
  steps = 0L
  val currents = next.keys.toArray.filter( node => node(node.length - 1) == 'A' )
  var j = 0
  val stepsForEach = Array.fill(currents.length)(0L)
  while (stepsForEach.exists( _ == 0L )) {
    j = 0
    steps += 1L
    while (j < currents.length) {
      currents(j) = nextNode(currents(j))
      if (stepsForEach(j) == 0L && currents(j)(currents(j).length - 1) == 'Z') {
        stepsForEach(j) = steps
      }
      j += 1
    }
    i += 1
    if (i == instructions.length) {
      i = 0
    }
  }
  
  val max = stepsForEach.max
  steps = max
  while (!stepsForEach.forall( steps % _ == 0L )) {
    steps += max
  }
  
  println("Steps to reach Z in all nodes: " + steps)
}