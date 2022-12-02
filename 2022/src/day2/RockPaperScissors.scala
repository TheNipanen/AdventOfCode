package day2

import scala.io.Source

object RockPaperScissors extends App {
  val s = Source.fromFile("src/day2/input.txt")
  val lines = try s.getLines().toArray.map(_.split(" ")) finally s.close()
  
  val sFromShape = Map("X" -> 1, "Y" -> 2, "Z" -> 3)
  val sFromOutcome = Map("X" -> Map("A" -> 3, "B" -> 0, "C" -> 6), "Y" -> Map("A" -> 6, "B" -> 3, "C" -> 0), "Z" -> Map("A" -> 0, "B" -> 6, "C" -> 3))
  def scoreFromRound1(opponent: String, me: String) = {
    val scoreFromShape = sFromShape(me)
    val scoreFromOutcome = sFromOutcome(me)(opponent)
    scoreFromShape + scoreFromOutcome
  }
  
  val shapeFromOutcome = Map("X" -> Map("A" -> "Z", "B" -> "X", "C" -> "Y"), "Y" -> Map("A" -> "X", "B" -> "Y", "C" -> "Z"), "Z" -> Map("A" -> "Y", "B" -> "Z", "C" -> "X"))
  def scoreFromRound2(opponent: String, outcome: String) = {
    val me = shapeFromOutcome(outcome)(opponent)
    val score = scoreFromRound1(opponent, me)
    score
  }
  
  var firstScore = 0
  var secondScore = 0
  var i = 0
  while (i < lines.length) {
    firstScore += scoreFromRound1(lines(i)(0), lines(i)(1))
    secondScore += scoreFromRound2(lines(i)(0), lines(i)(1))
    i += 1
  }
  
  println("First score: " + firstScore)
  println("Second score: " + secondScore)
}