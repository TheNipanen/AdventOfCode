package day14

import scala.io.Source

object RestroomRedoubt extends App {
  val s = Source.fromFile("src/day14/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val xLen = 101
  val yLen = 103
  
  class Robot(input: String) {
    val initialX = input.dropWhile( _ != '=' ).drop(1).takeWhile( _ != ',' ).toInt
    val initialY = input.dropWhile( _ != ',' ).drop(1).takeWhile( _ != ' ' ).toInt
  
    val velX = input.dropWhile( _ != 'v' ).drop(2).takeWhile( _ != ',' ).toInt
    val velY = input.dropWhile( _ != 'v' ).dropWhile( _ != ',' ).drop(1).toInt
    
    def positionAfter(rounds: Long): (Int, Int) = {
      var newX = (initialX.toLong + velX.toLong * rounds) % xLen
      var newY = (initialY.toLong + velY.toLong * rounds) % yLen
      if (newX < 0L) newX = xLen + newX
      if (newY < 0L) newY = yLen + newY
      (newX.toInt, newY.toInt)
    }
  }
  
  val robots = lines.map( new Robot(_) )
  val positions = robots.map( _.positionAfter(100) )
  
  val xMid = xLen / 2
  val yMid = yLen / 2
  
  def getCounts(positions: Array[(Int, Int)]): (Int, Int, Int, Int) = {
    var topLeft = 0
    var topRight = 0
    var botLeft = 0
    var botRight = 0
    var i = 0
    while (i < positions.length) {
      val pos = positions(i)
      if (pos._1 < xMid && pos._2 < yMid) topLeft += 1
      else if (pos._1 > xMid && pos._2 < yMid) topRight += 1
      else if (pos._1 < xMid && pos._2 > yMid) botLeft += 1
      else if (pos._1 > xMid && pos._2 > yMid) botRight += 1
      i+=1
    }
    (topLeft, topRight, botLeft, botRight)
  }
  
  def printRobots(positions: Array[(Int, Int)]) = {
    val set = scala.collection.mutable.HashSet[(Int, Int)]()
    positions.foreach( set(_) = true )
    for (y <- 0 until yLen) {
      var s = ""
      var x = 0
      while (x < xLen) {
        if (set((x,y))) s += '*'
        else s += ' '
        x += 1
      }
      println(s)
    }
  }
  
  val (topLeft, topRight, botLeft, botRight) = getCounts(positions)
  
  val safetyFactor = topLeft * topRight * botLeft * botRight
  println(safetyFactor)
  
  var i = 0
  while (i < 20000) {
    val positions = robots.map( _.positionAfter(i) )
    if (positions.distinct.length == positions.length) {
      println(i)
      printRobots(positions)
    }
    i += 1
  }
}