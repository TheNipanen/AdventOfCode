package day22

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object MonkeyMap extends App {
  val s = Source.fromFile("src/day22/input.txt.")
  val lines = try s.getLines().toArray finally s.close()
  
  var grid = ArrayBuffer[ArrayBuffer[Option[Char]]]()
  
  var i = 0
  val width = lines.dropRight(2).maxBy( _.length ).length
  val height = lines.length - 2
  while (lines(i) != "") {
    val line = lines(i)
    val gridLine = ArrayBuffer[Option[Char]]()
    var j = 0
    val lineWidth = line.length
    while (j < width) {
      if (j < lineWidth && line(j) != ' ') {
        val c = line(j)
        gridLine += Some(c)
      } else {
        gridLine += None
      }
      j += 1
    }
    grid += gridLine
    i += 1
  }
  
  i += 1
  val path = lines(i)
  
  val origY = 0
  val origX = grid(0).indexWhere( c => c.isDefined && c.get != '#' )
  
  var y = origY
  var x = origX
  var facing = (0, 1)
  val facMap = Map[(Int, Int), Char]( ((0,1), '>'), ((1,0),  'v'), ((0,-1), '<'), ((-1,0), '^') )
  
  def visualize() = {
    var res = ""
    for (yy <- 0 until height) {
      for (xx <- 0 until width) {
        if (grid(yy)(xx).isDefined) {
          res += grid(yy)(xx).get
        } else {
          res += " "
        }
      }
      res += "\n"
    }
    println(res)
  }
  
  def isInBounds(y: Int, x: Int) = {
    y >= 0 && y < height && x >= 0 && x < width && grid(y)(x).isDefined
  }
  
  // Hard-coded (unfortunately) cube transitions, no time to think of a general solution
  def mapNext(y: Int, x: Int) = {
    var nextY = y
    var nextX = x
    var nextFac = facing
    if (y == -1 && x >= 50 && x < 100) { // Top-middle -> up
      nextX = 0
      nextY = 100 + x
      nextFac = (0, 1)
    } else if (x == 49 && y < 50/* && facing == (0, -1)*/) { // Top-middle -> left
      nextX = 0
      nextY = 149 - y
      nextFac = (0, 1)
    } else if (y == -1 && x >= 100) { // Top-right -> up
      nextY = 199
      nextX = x - 100
    } else if (x == 150) { // Top-right -> right
      nextY = 149 - y
      nextX = 99
      nextFac = (0, -1)
    } else if (y == 50 && x >= 100 && facing == (1, 0)) { // Top-right -> down
      nextX = 99
      nextY = -50 + x
      nextFac = (0, -1)
    } else if (x == 49 && y >= 50 && y < 100 && facing == (0, -1)) { // MidUpper-middle -> left
      nextY = 100
      nextX = y - 50
      nextFac = (1, 0)
    } else if (x == 100 && y >= 50 && y < 100 && facing == (0, 1)) { //MidUpper-middle -> right
      nextY = 49
      nextX = y + 50
      nextFac = (-1, 0)
    } else if (x == 100 && y >= 100) { // MidLower-middle -> right
      nextX = 149
      nextY = 149 - y
      nextFac = (0, -1)
    } else if (y == 150 && x >= 50 && facing == (1, 0)) { // MidLower-middle -> down
      nextX = 49
      nextY = x + 100
      nextFac = (0, -1)
    } else if (x == -1 && y < 150) { // MidLower-left -> left
      nextX = 50
      nextY = 149 - y
      nextFac = (0, 1)
    } else if (y == 99 && x < 50 && facing == (-1, 0)) { // MidLower-left -> up
      nextX = 50
      nextY = 50 + x
      nextFac = (0, 1)
    } else if (x == -1 && y >= 150) { // Bot-left -> left
      nextY = 0
      nextX = y - 100
      nextFac = (1, 0)
    } else if (x == 50 && y >= 150 && facing == (0, 1)) { //Bot-left -> right
      nextY = 149
      nextX = y - 100
      nextFac = (-1, 0)
    } else if (y == 200) { // Bot-left -> down
      nextY = 0
      nextX = x + 100
      nextFac = (1, 0)
    }
    (nextY, nextX, nextFac)
  }
  
  def move(part1: Boolean): Unit = {
    var nextY = y + facing._1
    var nextX = x + facing._2
    var fac = facing
    if (part1) {
      if (!isInBounds(nextY, nextX)) {
        while (isInBounds(nextY - facing._1, nextX - facing._2)) {
          nextY -= facing._1
          nextX -= facing._2
        }
      }
    } else {
      val next = mapNext(nextY, nextX)
      nextY = next._1
      nextX = next._2
      fac = next._3
    }
    
    if (grid(nextY)(nextX).get != '#') {
      grid(y)(x) = Some(facMap(facing))
      y = nextY
      x = nextX
      facing = fac
    }
  }
  def move(amount: Int, part1: Boolean): Unit = {
    var moved = 0
    while (moved < amount) {
      move(part1)
      moved += 1
    }
  }
  
  def traverse(part1: Boolean) = {
    i = 0
    while (i < path.length) {
      if (path(i).isDigit) {
        var amount = ""
        while (i < path.length && path(i).isDigit) {
          amount += path(i)
          i += 1
        }
        move(amount.toInt, part1)
      } else {
        path(i) match {
          case 'L' => {
            val prevY = facing._1
            val nextY = -facing._2
            val nextX = prevY
            facing = (nextY, nextX)
          }
          case 'R' => {
            val prevY = facing._1
            val nextY = facing._2
            val nextX = -prevY
            facing = (nextY, nextX)
          }
        }
        i += 1
      }
    }
  }
  
  traverse(true)
  
  val row = y + 1
  val col = x + 1
  val fac = facing match { case (0, 1) => 0 case (1, 0) => 1 case (0, -1) => 2 case (-1, 0) => 3 case _ => 0 }
  val passwd = 1000 * row + 4 * col + fac
  
  println("Final password: " + passwd)
  //visualize()
  
  grid = grid.map( _.map( c => { if (c.isDefined && c.get != '#') Some('.') else c } ) )
  y = origY
  x = origX
  facing = (0, 1)
  traverse(false)
  
  val row2 = y + 1
  val col2 = x + 1
  val fac2 = facing match { case (0, 1) => 0 case (1, 0) => 1 case (0, -1) => 2 case (-1, 0) => 3 case _ => 0 }
  val passwd2 = 1000 * row2 + 4 * col2 + fac2
  
  println("Cube final password: " + passwd2)
}