package day15

import scala.io.Source
import scala.collection.mutable.HashMap

object WarehouseWoes extends App {
  val s = Source.fromFile("src/day15/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val grid = lines.takeWhile( _ != "" )
  val grid2 = grid.clone()
  val moves = lines.dropWhile( _ != "" ).drop(1).mkString
  
  val origY = grid.indexWhere( _.contains('@') )
  val origX = grid(origY).indexOf('@')
  var currentPos = (origY, origX)
  
  def moveOnlyRobot(cur: (Int, Int), next: (Int, Int), g: Array[String]) = {
    g(next._1) = g(next._1).updated(next._2, '@')
    g(cur._1) = g(cur._1).updated(cur._2, '.')
  }
  
  def printGrid(g: Array[String]) = {
    for (s <- g) {
      println(s)
    }
    println()
  }
  
  def move(currentPos: (Int, Int), c: Char): (Int, Int) = {
    val (dirY, dirX) = if (c == '^') (-1, 0) else if (c == '>') (0, 1) else if (c == '<') (0, -1) else (1, 0)
    val nextY = currentPos._1 + dirY
    val nextX = currentPos._2 + dirX
    var freeY = nextY
    var freeX = nextX
    while (grid(freeY)(freeX) == 'O') {
      freeY += dirY
      freeX += dirX
    }
    
    if (grid(freeY)(freeX) == '#') {
      return currentPos
    }
    
    grid(freeY) = grid(freeY).updated(freeX, grid(nextY)(nextX))
    moveOnlyRobot(currentPos, (nextY, nextX), grid)
    (nextY, nextX)
  }
  
  moves.foreach( m => currentPos = move(currentPos, m) )
  
  var sum = 0
  for (y <- 0 until grid.length; x <- 0 until grid(y).length) {
    val c = grid(y)(x)
    if (c == 'O') {
      sum += 100 * y + x
    }
  }
  
  println(sum)
  
  var i = 0
  while (i < grid2.length) {
    val line = grid2(i)
    var newLine = ""
    for (c <- line) {
      if (c == '#') newLine += "##"
      else if (c == 'O') newLine += "[]"
      else if (c == '.') newLine += ".."
      else if (c == '@') newLine += "@."
      else throw new Exception("Bad")
    }
    grid2(i) = newLine
    i += 1
  }
  
  val origY2 = grid2.indexWhere( _.contains('@') )
  val origX2 = grid2(origY2).indexOf('@')
  currentPos = (origY2, origX2)
  
  def isBox(c: Char) = {
    c == '[' || c == ']'
  }
  
  def move2(currentPos: (Int, Int), c: Char): (Int, Int) = {
    val (dirY, dirX) = if (c == '^') (-1, 0) else if (c == '>') (0, 1) else if (c == '<') (0, -1) else (1, 0)
    val nextY = currentPos._1 + dirY
    val nextX = currentPos._2 + dirX
    val cAtNext = grid2(nextY)(nextX)
    var boxes = HashMap[(Int, Int), Char]()
    if (isBox(cAtNext)) {
      var prevSize = 0
      boxes((nextY, nextX)) = cAtNext
      if (cAtNext == '[') boxes((nextY, nextX + 1)) = ']'
      else boxes((nextY, nextX - 1)) = '['
      while (prevSize != boxes.size) {
        prevSize = boxes.size
        val newBoxes = boxes.clone()
        boxes.foreach(pair => {
          val (boxY, boxX) = pair._1
          val aboveY = boxY + dirY
          val aboveX = boxX + dirX
          val cAbove = grid2(aboveY)(aboveX)
          if (isBox(cAbove)) {
            newBoxes((aboveY, aboveX)) = cAbove
            if (cAbove == '[') newBoxes((aboveY, aboveX + 1)) = ']'
            else newBoxes((aboveY, aboveX - 1)) = '['
          }
        })
        boxes = newBoxes
      }
    } 
    
    if (grid2(nextY)(nextX) == '#' || boxes.exists(pair => {
      val (boxY, boxX) = pair._1
      val aboveY = boxY + dirY
      val aboveX = boxX + dirX
      val cAbove = grid2(aboveY)(aboveX)
      cAbove == '#'
    })) {
      return currentPos
    }
    
    boxes.foreach(pair => {
      val (boxY, boxX) = pair._1
      grid2(boxY) = grid2(boxY).updated(boxX, '.')
    })
    boxes.foreach(pair => {
      val (boxY, boxX) = pair._1
      val aboveY = boxY + dirY
      val aboveX = boxX + dirX
      grid2(aboveY) = grid2(aboveY).updated(aboveX, pair._2)
    })
    moveOnlyRobot(currentPos, (nextY, nextX), grid2)
    (nextY, nextX)
  }
  
  var moved = 0
  moves.foreach( m => currentPos = move2(currentPos, m) )
  
  var sum2 = 0
  for (y <- 0 until grid2.length; x <- 0 until grid2(y).length) {
    val c = grid2(y)(x)
    if (c == '[') {
      sum2 += 100 * y + x
    }
  }
  
  println(sum2)
}