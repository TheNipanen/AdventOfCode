package day7

import scala.io.Source
import scala.collection.mutable.HashMap

object CamelCards extends App {
  val s = Source.fromFile("src/day7/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val order = HashMap('A' -> 0, 'K' -> 1, 'Q' -> 2, 'J' -> 3, 'T' -> 4, '9' -> 5, '8' -> 6, '7' -> 7, '6' -> 8, '5' -> 9, '4' -> 10, '3' -> 11, '2' -> 12)
  def handType(hand: String): Int = {
    val count = HashMap[Char, Int]()
    for (c <- hand) {
      if (!count.contains(c)) {
        count(c) = 0
      }
      count(c) += 1
    }
    if (count.size == 1) {
      return 0
    }
    if (count.size == 2) {
      if (count.keys.exists( count(_) == 4 )) {
        return 1
      }
      return 2
    }
    if (count.size == 3) {
      if (count.keys.exists( count(_) == 3 )) {
        return 3
      }
      return 4
    }
    if (count.size != 5) {
      return 5
    }
    return 6
  }
  
  val pairs = lines.map( _.split(" ") ).map( pair => (pair(0), pair(1).toLong) )
  val sorted = pairs.sortBy( pair => (handType(pair._1), order(pair._1(0)), order(pair._1(1)), order(pair._1(2)), order(pair._1(3)), order(pair._1(4))) )
  
  var winnings = 0L
  var rank = sorted.length
  var i = 0
  val l = sorted.length
  while (i < l) {
    winnings += rank * sorted(i)._2
    rank -= 1
    i += 1
  }
  
  println("Total winnings: " + winnings)
  
  order('J') = 13
  def handType2(hand: String): Int = {
    val cards = order.keys
    var best = Int.MaxValue
    for (c <- cards) {
      val t = handType(hand.map( card => if (card == 'J') c else card ))
      best = best min t
    }
    best
  }
  
  val sorted2 = pairs.sortBy( pair => (handType2(pair._1), order(pair._1(0)), order(pair._1(1)), order(pair._1(2)), order(pair._1(3)), order(pair._1(4))) )
  winnings = 0L
  rank = sorted2.length
  i = 0
  while (i < l) {
    winnings += rank * sorted2(i)._2
    rank -= 1
    i += 1
  }
  
  println("Total winnings with jokers: " + winnings)
}