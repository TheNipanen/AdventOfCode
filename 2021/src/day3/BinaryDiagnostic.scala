package day3

import scala.io.Source

object BinaryDiagnostic extends App {
  val s = Source.fromFile("src/day3/input.txt")
  val reader = s.bufferedReader()
  var line = reader.readLine()
  val n = line.length()
  var ones = new Array[Int](n)
  var zeros = new Array[Int](n)
  
  while (line != null) {
    var i = 0
    while (i < n) {
      if (line(i) == '1') {
        ones(i) += 1
      } else {
        zeros(i) += 1
      }
      i += 1
    }
    line = reader.readLine()
  }
  s.close()
  
  var gamma = 0
  var epsilon = 0
  var i = 0
  while (i < n) {
    val moreCommon = if (ones(i) > zeros(i)) 1 else 0
    val lessCommon = moreCommon ^ 1
    val shiftedMore = moreCommon << (n-i-1)
    val shiftedLess = lessCommon << (n-i-1)
    gamma = gamma | shiftedMore
    epsilon = epsilon | shiftedLess
    i += 1
  }
  
  val powerConsumption = gamma * epsilon
  println("Power consumption: " + powerConsumption)
  
  val ss = Source.fromFile("src/day3/input.txt")
  var ogNumbers = ss.getLines().toArray
  ss.close()
  var co2Numbers = ogNumbers.clone()
  i = 0
  while (i < n) {
    var ii = 0
    if (ogNumbers.size > 1) {
      ones = new Array[Int](n)
      zeros = new Array[Int](n)
      while (ii < ogNumbers.size) {
        var j = 0
        while (j < n) {
          if (ogNumbers(ii)(j) == '1') {
            ones(j) += 1
          } else {
            zeros(j) += 1
          }
          j += 1
        }
        ii += 1
      }
      val moreCommon = if (ones(i) >= zeros(i)) 1 else 0
      ogNumbers = ogNumbers.filter( _(i).asDigit == moreCommon )
    }
    if (co2Numbers.size > 1) {
      ii = 0
      ones = new Array[Int](n)
      zeros = new Array[Int](n)
      while (ii < co2Numbers.size) {
        var j = 0
        while (j < n) {
          if (co2Numbers(ii)(j) == '1') {
            ones(j) += 1
          } else {
            zeros(j) += 1
          }
          j += 1
        }
        ii += 1
      }
      val lessCommon = if (zeros(i) > ones(i)) 1 else 0
      co2Numbers = co2Numbers.filter( _(i).asDigit == lessCommon )
    }
    i += 1
  }
  
  val oxygenGeneratorRating = Integer.parseInt(ogNumbers(0), 2)
  val co2ScrubberRating = Integer.parseInt(co2Numbers(0), 2)
  val lifeSupportRating = oxygenGeneratorRating * co2ScrubberRating
  println("Life support rating: " + lifeSupportRating)
}