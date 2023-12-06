package day4

import scala.io.Source
import scala.collection.mutable.HashMap

object Scratchcards extends App {
  val s = Source.fromFile("src/day4/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  var totalPoints = 0
  
  val matchingNumbers = HashMap[Int, Int]()
  val copies = HashMap[Int, Int]()
  
  var maxCard = 0
  var i = 0
  val l = lines.length
  while (i < l) {
    var line = lines(i)
    val cardNumber = line.dropWhile( !_.isDigit ).takeWhile( _.isDigit ).toInt
    copies(cardNumber) = 1
    maxCard = maxCard max cardNumber
    line = line.drop(9)
    
    val (winningN, yourN) = line.splitAt(line.indexOf('|'))
    val winningNumbers = winningN.filter( _ != '|' ).trim().replace("  ", " ").split(" ").map( _.toInt )
    val yourNumbers = yourN.filter( _ != '|' ).trim().replace("  ", " ").split(" ").map( _.toInt )
    
    var points = 0
    var matching = 0
    for (n <- yourNumbers) {
      if (winningNumbers.contains(n)) {
        if (points == 0) {
          points += 1
        } else {
          points *= 2
        }
        matching += 1
      }
    }
    totalPoints += points
    matchingNumbers(cardNumber) = matching
    
    i += 1
  }
  
  i = 1
  while (i <= maxCard) {
    val matching = matchingNumbers(i)
    val copiesOfThis = copies(i)
    var j = i + 1
    while (j <= i + matching) {
      copies(j) += copiesOfThis
      j += 1
    }
    i += 1
  }
  val amount = copies.values.sum
  
  println("Total points: " + totalPoints)
  println("Total scratchcards: " + amount)
}