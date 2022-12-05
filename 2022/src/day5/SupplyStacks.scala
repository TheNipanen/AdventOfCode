package day5

import scala.io.Source
import scala.collection.mutable.{ ArrayBuffer, Stack }

object SupplyStacks extends App {
  val s = Source.fromFile("src/day5/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  val stackLines = lines.takeWhile( _.nonEmpty )
  
  val stacks = ArrayBuffer[Stack[Char]]()
  var i = 1
  val bottomLine = stackLines(stackLines.length - 1)
  while (i < bottomLine.length) {
    assert(bottomLine(i).isDigit)
    stacks += Stack()
    i += 4
  }
  
  var lineIndex = stackLines.length - 2
  while (lineIndex >= 0) {
    i = 1
    var stackIndex = 0
    val line = stackLines(lineIndex)
    while (i < line.length) {
      if (!line(i).isSpaceChar) {
        stacks(stackIndex).push(line(i))
      }
      stackIndex += 1
      i += 4
    }
    lineIndex -= 1
  }
  val stacks2 = stacks.map( _.clone() )
  
  val moveLines = lines.dropWhile( _.nonEmpty ).drop(1)
  lineIndex = 0
  while (lineIndex < moveLines.length) {
    val line = moveLines(lineIndex)
    val move = line.drop(5).takeWhile( _.isDigit ).toInt
    val rest = line.drop(5).dropWhile( _.isDigit ).drop(6)
    val from = rest(0).asDigit - 1
    val to = rest(5).asDigit - 1
    
    i = 0
    val temp = Stack[Char]()
    while (i < move) {
      val c = stacks(from).pop()
      stacks(to).push(c)
      val c2 = stacks2(from).pop()
      temp.push(c2)
      i += 1
    }
    while (temp.nonEmpty) {
      stacks2(to).push(temp.pop())
    }
    
    lineIndex += 1
  }
  
  var tops = ""
  for (s <- stacks) {
    tops += s.top
  }
  println(tops)
  var tops2 = ""
  for (s <- stacks2) {
    tops2 += s.top
  }
  println(tops2)
}