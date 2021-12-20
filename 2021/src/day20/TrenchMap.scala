package day20

import scala.io.Source
import scala.collection.mutable.{ HashSet, ArrayBuffer }

object TrenchMap extends App {
  val s = Source.fromFile("src/day20/input.txt")
  val a = s.getLines().toArray
  s.close()
  val algorithm = a(0)
  val image = a.drop(2)
  
  var litPixels = HashSet[(Int,Int)]()
  var xMin = -1
  var xMax = image(0).size
  var yMin = -1
  var yMax = image.size
  for (i <- 0 until image.size) {
    for (j <- 0 until image(0).size) {
      if (image(i)(j) == '#') litPixels((j,i)) = true
    }
  }
  
  def neighbors(x: Int, y: Int): Array[(Int,Int)] = {
    Array((x-1,y-1),(x,y-1),(x+1,y-1),(x-1,y),(x,y),(x+1,y),(x-1,y+1),(x,y+1),(x+1,y+1))
  }
  def printImage() = {
    for (x <- xMin to xMax) {
      for (y <- yMin to yMax) {
        print(if (litPixels((x,y))) '#' else '.')
      }
      println()
    }
    println()
  }
  
  //printImage()
  var steps = 0
  while (steps < 50) {
    val next = HashSet[(Int,Int)]()
    for (x <- xMin to xMax) {
      for (y <- yMin to yMax) {
        val neigh = neighbors(x, y)
        var num = 0
        for (i <- 0 until 9) {
          val (xx,yy) = neigh(i)
          val bit = if (litPixels((xx,yy)) || ((xx <= xMin || xx >= xMax || yy <= yMin || yy >= yMax) && steps % 2 != 0)) 1 else 0
          val shifted = bit << (8-i)
          num |= shifted
        }
        val sq = algorithm(num)
        if (sq == '#') next((x,y)) = true
      }
    }
    litPixels = next
    //printImage()
    if (steps == 2) println("Number of lit pixels after 2 steps: " + litPixels.size)
    xMin -= 1
    xMax += 1
    yMin -= 1
    yMax += 1
    steps += 1
  }
  
  println("Number of lit pixels after 50 steps: " + litPixels.size)
}