package day6

import scala.io.Source
import scala.collection.mutable.HashSet

object GuardGallivant extends App {
  val s = Source.fromFile("src/day6/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val visited = HashSet[(Int, Int)]()
  val origY = lines.indexWhere( _.contains("^") )
  val origX = lines(origY).indexOf('^') 
  var y = origY
  var x = origX
  
  var direction = (-1, 0) 
  def nextDirection(dir: (Int, Int)): (Int, Int) = {
    dir match {
      case (-1, 0) => (0, 1)
      case (0, 1)  => (1, 0)
      case (1, 0)  => (0, -1)
      case (0, -1) => (-1, 0)
      case _       => throw new Exception("Bad direction")
    }
  }
  
  def isIn(y: Int, x: Int): Boolean = {
    x >= 0 && x < xLen && y >= 0 && y < yLen
  }
  
  def nextPos(y: Int, x: Int): (Int, Int) = {
    var nextY = y + direction._1
    var nextX = x + direction._2
    while (isIn(nextY, nextX) && lines(nextY)(nextX) == '#') {
      direction = nextDirection(direction)
      nextY = y + direction._1
      nextX = x + direction._2
    }
    (nextY, nextX)
  }
  
  val yLen = lines.length
  val xLen = lines(0).length
  var printCount = 0
  while (isIn(y, x)) {
    visited((y, x)) = true
    val (nextY, nextX) = nextPos(y, x)
    y = nextY
    x = nextX
  }
  
  println(visited.size)
  
  def tryLoop(obsY: Int, obsX: Int): Boolean = {
    if (lines(obsY)(obsX) != '.') {
      return false
    }
    
    lines(obsY) = lines(obsY).updated(obsX, '#')
    
    y = origY
    x = origX
    direction = (-1, 0)
    val visited = HashSet[(Int, Int, Int, Int)]()
    
    while (isIn(y, x)) {
      if (visited((y, x, direction._1, direction._2))) {
        lines(obsY) = lines(obsY).updated(obsX, '.')
        return true
      }
      visited((y, x, direction._1, direction._2)) = true
      val (nextY, nextX) = nextPos(y, x)
      y = nextY
      x = nextX
    }
    lines(obsY) = lines(obsY).updated(obsX, '.')
    false
  }
  
  var obsPositions = 0
  for (y <- 0 until yLen) {
    for (x <- 0 until xLen) {
      if (tryLoop(y, x)) {
        obsPositions += 1
      }
    }
  }
  println(obsPositions)
}