package day24

import scala.io.Source

object LobbyLayout extends App {
  val s = Source.fromFile("src/day24/input.txt")
  val r = s.bufferedReader()
  
  val color = scala.collection.mutable.HashMap[(Int,Int), Boolean]()
  def parse(s: String) = {
    var i = 0
    var x = 0
    var y = 0
    while (i < s.length) {
      val c = s(i)
      c match {
        case 'w' => {
          x -= 2
          i += 1
        }
        case 'e' => {
          x += 2
          i += 1
        }
        case 'n' | 's' => {
          if (c == 'n') y += 1
          else y -= 1
          val c2 = s(i+1)
          c2 match {
            case 'w' => x -= 1
            case 'e' => x += 1
          }
          i += 2
        }
      }
    }
    val coord = (x,y)
    if (color.contains(coord)) color(coord) = !color(coord)
    else color(coord) = true
  }
  var line = r.readLine()
  while (line != null) {
    parse(line)
    line = r.readLine()
  }
  s.close()
  val blacks = color.values.filter( b => b ).size
  println("Black tiles: " + blacks)
  def limits = {
    var xMin, yMin = Int.MaxValue; var xMax, yMax = Int.MinValue
    for ((x, y) <- color.keys) {
      xMin = xMin min x
      yMin = yMin min y
      xMax = xMax max x
      yMax = yMax max y
    }
    (xMin - 2, xMax + 2, yMin - 1, yMax + 1)
  }
  def neighbours(x: Int, y: Int) = {
    Array((x-1,y+1),(x-2,y),(x-1,y-1),(x+1,y+1),(x+2,y),(x+1,y-1))
  }
  def countBlacksNear(x: Int, y: Int) = {
    val neigh = neighbours(x,y)
    var count = 0
    for (coord <- neigh) {
      if (color.contains(coord) && color(coord)) count += 1
    }
    count
  }
  def iteration = {
    val (xMin, xMax, yMin, yMax) = limits
    val next = color.clone()
    for (x <- xMin to xMax; y <- yMin to yMax; if (!(y % 2 == 0 && (x % 2 == -1 || x % 2 == 1)))) {
      val count = countBlacksNear(x,y)
      val coord = (x,y)
      if (color.contains(coord) && color(coord)) {
        if (count == 0 || count > 2) next(coord) = false
        else next(coord) = true
      } else {
        if (count == 2) next(coord) = true
        else next(coord) = false
      }
    }
    next.foreach( p => color(p._1) = p._2 )
    iterations += 1
  }
  var iterations = 0
  while (iterations < 100) {
    iteration
  }
  val blacks2 = color.values.filter( b => b ).size
  println("Black tiles after 100 days: " + blacks2)
}