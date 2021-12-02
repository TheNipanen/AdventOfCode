package day8

import scala.io.Source

object HandheldHalting extends App {
  val s = Source.fromFile("src/day8/input.txt")
  val lines = s.getLines().toArray
  val n = lines.length
  
  val visited = new Array[Boolean](n)
  var accumulator = 0
  var i = 0
  
  def parseInstr(s: String): (String, Int) = {
    val ins = s.take(3)
    val num = s.drop(4).toInt
    return (ins, num)
  }
  
  while (i < n) {
    if (visited(i)) {
      i = n
    } else {
      val line = lines(i)
      val (ins, num) = parseInstr(line)
      visited(i) = true
      ins match {
        case "acc" => {
          accumulator += num
          i += 1
        }
        case "jmp" => {
          i += num
        }
        case "nop" => {
          i += 1
        }
      }
    }
  }
  println("Accumulator value: " + accumulator)
  
  var found = false
  var change = -1
  var first = true
  while (!found) {
    var ii = change + 1
    var reached = false
    while (!reached) {
      val line = lines(ii)
      val (ins, num) = parseInstr(line)
      if (ins == "jmp" || ins == "nop") {
        reached = true
        change = ii
        //println(ins + ":" + num + ":" + (ii))
      }
      ii += 1
    }
    accumulator = 0
    (0 until n).foreach(visited(_) = false)
    i = 0
    while (i < n) {
      //if (first) println(i)
      if (visited(i)) i = n
      else {
        val line = lines(i)
        visited(i) = true
        val (ins, num) = parseInstr(line)
        ins match {
          case "acc" => {
            accumulator += num
            i += 1
          }
          case "jmp" => {
            if (change == i) i += 1
            else i += num
          }
          case "nop" => {
            if (change == i) i += num
            else i += 1
          }
        }
        if (i >= n) found = true
      }
    }
    first = false
  }
  println("Accumulator value: " + accumulator)
}