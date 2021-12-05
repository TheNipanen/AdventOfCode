package day5

import scala.io.Source

object HydrothermalVenture  extends App {
  val s = Source.fromFile("src/day5/input.txt")
  val lines = s.getLines().toArray.map(l => {val s = l.split(" -> ").map(_.split(",")); ((s(0)(0).toInt, s(0)(1).toInt), (s(1)(0).toInt, s(1)(1).toInt))})
  val horOrVerLines = lines.filter( l => l._1._1 == l._2._1 || l._1._2 == l._2._2 )
  var maxX = 0
  var maxY = 0
  for (l <- horOrVerLines) {
    val ((x1,y1),(x2,y2)) = l
    maxX = x1 max x2 max maxX
    maxY = y1 max y2 max maxY
  }
  var grid = Array.ofDim[Int](maxX+1, maxY+1)
  for (l <- horOrVerLines) {
    val ((x1,y1),(x2,y2)) = l
    if (x1 == x2) {
      val smallY = y1 min y2
      val bigY = y1 max y2
      for (y <- smallY to bigY) {
        grid(x1)(y) += 1
      }
    } else {
      val smallX = x1 min x2
      val bigX = x1 max x2
      for (x <- smallX to bigX) {
        grid(x)(y1) += 1
      }
    }
  }
  
  var dangerousPoints = 0
  for (x <- 0 to maxX; y <- 0 to maxY) {
    if (grid(x)(y) >= 2) {
      dangerousPoints += 1
    }
  }
  println("Dangerous points: " + dangerousPoints)
  
  maxX = 0
  maxY = 0
  for (l <- lines) {
    val ((x1,y1),(x2,y2)) = l
    maxX = x1 max x2 max maxX
    maxY = y1 max y2 max maxY
  }
  grid = Array.ofDim[Int](maxX+1, maxY+1)
  for (l <- lines) {
    val ((x1,y1),(x2,y2)) = l
    if (x1 == x2) {
      val smallY = y1 min y2
      val bigY = y1 max y2
      for (y <- smallY to bigY) {
        grid(x1)(y) += 1
      }
    } else if (y1 == y2) {
      val smallX = x1 min x2
      val bigX = x1 max x2
      for (x <- smallX to bigX) {
        grid(x)(y1) += 1
      }
    } else {
      val smallX = x1 min x2
      val bigX = x1 max x2
      var y = y2
      var increaseY = y1 > y2
      if (smallX == x1) {
        y = y1
        increaseY = y2 > y1
      }
      for (x <- smallX to bigX) {
        grid(x)(y) += 1
        if (increaseY) y += 1
        else y -= 1
      }
    }
  }
  
  dangerousPoints = 0
  for (x <- 0 to maxX; y <- 0 to maxY) {
    if (grid(x)(y) >= 2) {
      dangerousPoints += 1
    }
  }
  println("Dangerous points with diagonal lines: " + dangerousPoints)
}