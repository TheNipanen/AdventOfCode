package day10

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object SyntaxScoring extends App {
  val s = Source.fromFile("src/day10/input.txt")
  val lines = s.getLines().toArray
  s.close()
  
  def end(c: Char) = {
    c match {
      case '(' => ')'
      case '[' => ']'
      case '{' => '}'
      case '<' => '>'
    }
  }
  def penalty(c: Char) = {
    c match {
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case '>' => 25137
    }
  }
  def value(c: Char) = {
    c match {
      case ')' => 1L
      case ']' => 2L
      case '}' => 3L
      case '>' => 4L
    }
  }
  val startChars = Array('(','[','{','<')
  
  var sum = 0
  var i = 0
  while (i < lines.size) {
    val line = lines(i)
    val starts = ArrayBuffer[Char]()
    var j = 0
    while (j < line.size) {
      val c = line(j)
      if (startChars.contains(c)) {
        starts += c
      } else {
        if (starts.size == 0 || end(starts(starts.size-1)) != c) {
          sum += penalty(c)
          j = line.size
          lines(i) = null
        } else {
          starts.remove(starts.size-1)
        }
      }
      j += 1
    }
    i += 1
  }
  println("Syntax error score: " + sum)
  
  val incompleteLines = lines.filter( _ != null )
  val scores = ArrayBuffer[Long]()
  i = 0
  while (i < incompleteLines.size) {
    val line = incompleteLines(i)
    val starts = ArrayBuffer[Char]()
    var j = 0
    while (j < line.size) {
      val c = line(j)
      if (startChars.contains(c)) {
        starts += c
      } else {
        starts.remove(starts.size-1)
      }
      j += 1
    }
    val values = starts.toArray.map( c => value(end(c)) ).reverse
    j = 0
    var score = 0L
    while (j < values.size) {
      score *= 5L
      score += values(j)
      j += 1
    }
    scores += score
    i += 1
  }
  
  val sortedScores = scores.toArray.sorted
  val middleScore = sortedScores(sortedScores.size/2)
  println("Middle score: " + middleScore)
}