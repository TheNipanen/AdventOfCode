package day14

import scala.io.Source
import scala.collection.mutable.HashMap

object RegolithReservoir extends App {
  val s = Source.fromFile("src/day14/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val grid = HashMap[(Int, Int), Char]()
  var i = 0
  var maxY = 0
  var minX = 500
  var maxX = 500
  while(i < lines.length) {
    val line = lines(i)
    val parts = line.split(" -> ")
    for (j <- 1 until parts.length) {
      val prev = parts(j - 1).split(",").map( _.toInt )
      val cur = parts(j).split(",").map( _.toInt )
      maxY = maxY max prev(1) max cur(1)
      maxX = maxX max prev(0) max cur(0)
      minX = minX min prev(0) min cur(0)
      var ii = prev(0)
      var jj = prev(1)
      val deltaI = if (ii == cur(0)) 0 else if (ii < cur(0)) 1 else -1
      val deltaJ = if (jj == cur(1)) 0 else if (jj < cur(1)) 1 else -1
      while (ii != cur(0) || jj != cur(1)) {
        grid((ii, jj)) = '#'
        ii += deltaI
        jj += deltaJ
      }
      grid((ii, jj)) = '#'
    }
    i += 1
  }
  
  def visualizeGrid(part2: Boolean) = {
    print("   ")
    for (x <- minX to maxX) {
      print(x.toString()(0))
    }
    println()
    print("   ")
    for (x <- minX to maxX) {
      print(x.toString()(1))
    }
    println()
    print("   ")
    for (x <- minX to maxX) {
      print(x.toString()(2))
    }
    println()
    for (y <- 0 to maxY) {
      print(y)
      if (y < 10) {
        print("  ")
      } else if (y < 100) {
        print(" ")
      }
      for (x <- minX to maxX) {
        val c = grid.get((x, y))
        if (part2 && y == maxY) {
          print("#")
        } else if ((x, y) == (500, 0)) {
          print("+")
        } else if (c.isDefined) {
          print(c.get)
        } else {
          print(".")
        }
      }
      println()
    }
    println()
  }
  
  //visualizeGrid(false)
  
  var continue = true
  var sandAtRest = 0
  while (continue) {
    var sand = (500, 0)
    var falling = true
    while (falling) {
      val test = grid.get((sand._1, sand._2 + 1))
      val test2 = grid.get((sand._1 - 1, sand._2 + 1))
      val test3 = grid.get((sand._1 + 1, sand._2 + 1))
      val next = if (test.isEmpty) (sand._1, sand._2 + 1) else if (test2.isEmpty) (sand._1 - 1, sand._2 + 1) else if (test3.isEmpty) (sand._1 + 1, sand._2 + 1) else sand
      if (next == sand) {
        grid(sand) = 'o'
        sandAtRest += 1
        falling = false
      } else {
        sand = next
        falling = sand._2 < maxY
      }
    }
    continue = sand._2 < maxY
  }
  
  //visualizeGrid(false)
  
  println("Sand at rest before falling into the abyss: " + sandAtRest)
  
  grid.foreach( v => {if (v._2 == 'o') grid.remove(v._1)} )
  continue = true
  sandAtRest = 0
  maxY += 2
  //visualizeGrid(true)
  while (continue) {
    var sand = (500, 0)
    var falling = true
    while (falling) {
      val test = grid.get((sand._1, sand._2 + 1))
      val test2 = grid.get((sand._1 - 1, sand._2 + 1))
      val test3 = grid.get((sand._1 + 1, sand._2 + 1))
      val next = if (test.isEmpty) (sand._1, sand._2 + 1) else if (test2.isEmpty) (sand._1 - 1, sand._2 + 1) else if (test3.isEmpty) (sand._1 + 1, sand._2 + 1) else sand
      if (next == sand || next._2 == maxY) {
        grid(sand) = 'o'
        sandAtRest += 1
        falling = false
      } else {
        sand = next
      }
    }
    continue = sand != (500, 0)
  }
  
  //visualizeGrid(true)
  
  println("Sand at rest before source becomes blocked: " + sandAtRest)
}