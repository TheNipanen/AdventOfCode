package day2

import scala.io.Source

object CubeConundrum extends App {
  val s = Source.fromFile("src/day2/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val reds = 12
  val greens = 13
  val blues = 14
  var sum = 0
  var powerSum = 0
  
  var i = 0
  val l = lines.length
  while (i < l) {
    var line = lines(i).drop(5)
    val id = line.takeWhile(_.isDigit).toInt
    line = line.dropWhile(_.isDigit).drop(1)
    val sets = line.split(";").map( _.filter( _ != ' ' ) )
    var possible = true
    var minReds = 0
    var minGreens = 0
    var minBlues = 0
    for (set <- sets) {
      for (setPart <- set.split(",")) {
        val amount = setPart.takeWhile( _.isDigit ).toInt
        val c = setPart.dropWhile( _.isDigit )(0)
        val colors = c match { case 'r' => reds case 'g' => greens case 'b' => blues}
        if (amount > colors) {
          possible = false
        }
        c match {
          case 'r' => minReds = minReds max amount
          case 'g' => minGreens = minGreens max amount
          case 'b' => minBlues = minBlues max amount
        }
      }
    }
    if (possible) {
      sum += id
    }
    val power = minReds * minGreens * minBlues
    powerSum += power
    i += 1
  }
  
  println("Sum of possible game ids: " + sum)
  println("Sum of game powers: " + powerSum)
}