package day13

import scala.io.Source

object ClawContraption extends App {
  val s = Source.fromFile("src/day13/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  def extract(line: String): (Int, Int) = {
    val first = line.dropWhile( !_.isDigit ).takeWhile( _.isDigit ).toInt
    val second = line.dropWhile( !_.isDigit ).dropWhile( _.isDigit ).dropWhile( !_.isDigit ).toInt
    (first, second)
  }
  
  var fewestTokens = 0
  var fewestTokensLong = 0L
  
  val l = lines.length
  var i = 0
  while (i < l) {
    val (aX, aY) = extract(lines(i))
    val (bX, bY) = extract(lines(i+1))
    val (targetX, targetY) = extract(lines(i+2))
    
    val possibleWins = (for (a <- 0 to 100; b <- 0 to 100; if (a * aX + b * bX == targetX && a * aY + b * bY == targetY)) yield (a, b))
    if (possibleWins.nonEmpty) {
      val cheapestWin = possibleWins.map( pair => pair._1 * 3 + pair._2 ).min
      fewestTokens += cheapestWin
    }
    
    val tX = targetX + 10000000000000L
    val tY = targetY + 10000000000000L
    
    /** Math for the second part
     *  a*aX + b*bX = tX
		 *  a*aY + b*bY = tY
     *
     *  b = (tY-a*aY)/bY
     *  a = (tX - tY*bX/bY)/(aX - aY*bX/bY)
    */
    
    if (bY != 0 && aX - aY * bX / bY != 0) {
      val a = (tX.toDouble - tY.toDouble * bX.toDouble / bY.toDouble) / (aX.toDouble - aY.toDouble * bX.toDouble / bY.toDouble)
      val b = (tY.toDouble - a * aY.toDouble) / bY.toDouble
      val rA = math.round(a)
      val rB = math.round(b)
      if (rA >= 0 && rB >= 0 && rA * aX.toLong + rB * bX.toLong == tX && rA * aY.toLong + rB * bY.toLong == tY) {
        fewestTokensLong += rA * 3L + rB
      }
    }
    
    i += 4
  }
  
  println(fewestTokens)
  println(fewestTokensLong)
}