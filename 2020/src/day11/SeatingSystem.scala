package day11

import scala.io.Source

object SeatingSystem extends App {
  val s = Source.fromFile("src/day11/input.txt")
  val orig = s.getLines().toArray.map( _.toCharArray() )
  s.close()
  var current = orig.clone().map( _.clone() )
  var next = current.clone().map( _.clone() )
  val yL = current.length
  val xL = current(0).length
  
  def adj(x: Int, y: Int): Array[(Int, Int)] = {
    val xMin = 0 max (x-1)
    val xMax = (xL-1) min (x+1)
    val yMin = 0 max (y-1)
    val yMax = (yL-1) min (y+1)
    (for (xx <- xMin to xMax; yy <- yMin to yMax if (!(xx == x && yy == y))) yield (xx, yy)).toArray
  }
  def isEq(a: Array[Array[Char]], b: Array[Array[Char]]) = {
    val aa = a.flatten
    val bb = b.flatten
    (0 until aa.length).forall( i => aa(i) == bb(i) )
  }
  
  var found = false
  while (!found) {
    for (y <- 0 until yL; x <- 0 until xL) {
      val c = current(y)(x)
      val adjacents = adj(x,y)
      c match {
        case 'L' => {
          if (adjacents.count( coord => current(coord._2)(coord._1) == '#' ) == 0) next(y)(x) = '#'
          else next(y)(x) = 'L'
        }
        case '#' => {
          if (adjacents.count( coord => current(coord._2)(coord._1) == '#' ) >= 4) next(y)(x) = 'L'
          else next(y)(x) = '#'
        }
        case _ => {}
      }
    }
    if (isEq(current,next)) found = true
    else {
      val temp = current
      current = next
      next = temp
    }
  }
  val count = next.flatten.count( _ == '#' )
  println("Occupied seats: " + count)
  
  def adj2(x: Int, y:Int): Array[(Int, Int)] = {
    val variations = Array((-1,-1),(0,-1),(1,-1),(-1,0),(1,0),(-1,1),(0,1),(1,1))
    val adjacents = scala.collection.mutable.ArrayBuffer[(Int,Int)]()
    for ((xV,yV) <- variations) {
      var xC = x
      var yC = y
      var found = false
      while (!found) {
        val xN = xC + xV
        val yN = yC + yV
        if (xN < 0 || xN >= xL || yN < 0 || yN >= yL || (current(yC)(xC) == '#' && (yC != y || xC != x)) || (current(yC)(xC) == 'L' && (yC != y || xC != x))) found = true
        else {
          xC = xN
          yC = yN
        }
      }
      if (xC != x || yC != y) adjacents += ((xC, yC))
    }
    return adjacents.toArray
  }
  
  current = orig.clone().map( _.clone() )
  found = false
  while (!found) {
    for (y <- 0 until yL; x <- 0 until xL) {
      //println(y + ":" + x)
      val c = current(y)(x)
      val adjacents = adj2(x,y)
      c match {
        case 'L' => {
          if (adjacents.count( coord => current(coord._2)(coord._1) == '#' ) == 0) next(y)(x) = '#'
          else next(y)(x) = 'L'
        }
        case '#' => {
          if (adjacents.count( coord => current(coord._2)(coord._1) == '#' ) >= 5) next(y)(x) = 'L'
          else next(y)(x) = '#'
        }
        case _ => {}
      }
    }
    if (isEq(current,next)) found = true
    else {
      val temp = current
      current = next
      next = temp
    }
  }
  val count2 = next.flatten.count( _ == '#' )
  println("Occupied seats again: " + count2)
}