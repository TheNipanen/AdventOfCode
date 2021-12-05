package day4

import scala.io.Source
import scala.collection.mutable.{ ArrayBuffer, HashSet }

object GiantSquid extends App {
  val s = Source.fromFile("src/day4/input.txt")
  val reader = s.bufferedReader()
  val numbers = reader.readLine().split(",").map( _.toInt )
  reader.readLine()
  var line = reader.readLine()
  val n = line.split(" ").size
  val b = ArrayBuffer[Array[Array[Int]]]()
  
  while (line != null) {
    val board = ArrayBuffer[Array[Int]]()
    while (line != "" && line != null) {
      board += line.trim.split(" +").map( _.toInt )
      line = reader.readLine()
    }
    b += board.toArray
    line = reader.readLine()
  }
  s.close()
  val boards = b.toArray
  
  val markedNumbers = HashSet[Int]()
  def boardWins(board: Array[Array[Int]]) = {
    board.exists( _.forall( markedNumbers(_) ) ) || board.transpose.exists( _.forall( markedNumbers(_) ) )
  }
  
  var i = 0
  val winningBoards = HashSet[Int]()
  while (i < numbers.size) {
    val number = numbers(i)
    markedNumbers(number) = true
    var j = 0
    while (j < boards.size) {
      val board = boards(j)
      if (!winningBoards(j) && boardWins(board)) {
        winningBoards(j) = true
        val unmarkedSum = board.flatten.filterNot( markedNumbers(_) ).sum
        val score = unmarkedSum * number
        println("Winning board score: " + score)
      }
      j += 1
    }
    i += 1
  }
  // These should be equal
  println(winningBoards.size)
  println(boards.size)
}