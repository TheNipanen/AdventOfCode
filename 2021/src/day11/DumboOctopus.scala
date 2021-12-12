package day11

import scala.io.Source

object DumboOctopus extends App {
  val s = Source.fromFile("src/day11/input.txt")
  var grid = s.getLines.toArray.map( _.split("").map( _.toInt ) )
  s.close()
  var nextGrid = Array.ofDim[Int](10,10)
  var flashes = 0
  
  def checkFlash(i: Int, j: Int, flashed: Array[Array[Boolean]]): Unit = {
    val neighbors = Array((i-1,j-1),(i-1,j),(i-1,j+1),(i,j-1),(i,j+1),(i+1,j-1),(i+1,j),(i+1,j+1)).filter( coords => coords._1 >= 0 && coords._1 < 10 && coords._2 >= 0 && coords._2 < 10 )
    for (n <- neighbors) {
      nextGrid(n._1)(n._2) += 1
      if (!flashed(n._1)(n._2) && nextGrid(n._1)(n._2) > 9) {
        flashes += 1
        flashed(n._1)(n._2) = true
        checkFlash(n._1, n._2, flashed)
      }
    }
  }
  
  var steps = 0
  while (steps < 214) {
    var i = 0
    while (i < 10) {
      var j = 0
      while (j < 10) {
        nextGrid(i)(j) = grid(i)(j) + 1
        j += 1
      }
      i += 1
    }
    
    val flashed = Array.ofDim[Boolean](10,10)
    i = 0
    while (i < 10) {
      var j = 0
      while (j < 10) {
        if (!flashed(i)(j) && nextGrid(i)(j) > 9) {
          flashes += 1
          flashed(i)(j) = true
          checkFlash(i, j, flashed)
        }
        j += 1
      }
      i += 1
    }
    
    i = 0
    while (i < 10) {
      var j = 0
      while (j < 10) {
        if (nextGrid(i)(j) > 9) {
          nextGrid(i)(j) = 0
        }
        j += 1
      }
      i += 1
    }
    if (nextGrid.flatten.forall( _ == 0 )) {
      println("Simulataneous flash at step " + (steps + 1)) 
    }
    
    val temp = grid
    grid = nextGrid
    nextGrid = temp
    steps += 1
    if (steps == 100) {
      println("Flashes after 100 steps: " + flashes)
    }
  }
  
  
}