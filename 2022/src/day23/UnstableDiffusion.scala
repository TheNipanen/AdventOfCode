package day23

import scala.io.Source
import scala.collection.mutable.{ HashMap, HashSet }

object UnstableDiffusion extends App {
  val s = Source.fromFile("src/day23/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val grid = HashSet[(Int, Int)]()
  
  var i = 0
  while (i < lines.length) {
    val line = lines(i)
    var j = 0
    while (j < line.length) {
      val c = line(j)
      if (c == '#') {
        grid((j,i)) = true
      }
      j += 1
    }
    i += 1
  }
  
  def neighbors(coord: (Int, Int)) = {
    for (x <- coord._1 - 1 to coord._1 + 1; y <- coord._2 - 1 to coord._2 + 1 if (x != coord._1 || y != coord._2)) yield (x, y)
  }
  
  def plus(c1: (Int, Int), c2: (Int, Int)): (Int, Int) = {
    (c1._1 + c2._1, c1._2 + c2._2)
  }
  
  var minX = Int.MaxValue
  var maxX = Int.MinValue
  var minY = Int.MaxValue
  var maxY = Int.MinValue
  
  var q = Vector[(Int, Int)]((-1,-1), (0,-1), (1,-1), (-1,1), (0,1), (1,1), (-1,-1), (-1,0), (-1,1), (1,-1), (1,0), (1,1))
  var rounds = 0
  var continue = true
  while (continue) {
    val proposals = HashMap[(Int, Int), Int]() // How many proposals per coordinate
    val proposal = HashMap[(Int, Int), (Int, Int)]() // The proposals of each elf
    
    val firstThree = q.take(3)
    val rest = q.drop(3)
    continue = false
    for (elfCoords <- grid) {
      var prop = (0,0)
      if (neighbors(elfCoords).forall( !grid(_) )) {
        prop = elfCoords
      } else if (firstThree.forall( c => !grid(plus(elfCoords, c)) )) {
        prop = plus(elfCoords, q(1))
      } else if (rest.take(3).forall( c => !grid(plus(elfCoords, c)) )) {
        prop = plus(elfCoords, q(4))
      } else if (q.drop(6).take(3).forall( c => !grid(plus(elfCoords, c)) )) {
        prop = plus(elfCoords, q(7))
      } else if (q.drop(9).forall( c => !grid(plus(elfCoords, c)) )) {
        prop = plus(elfCoords, q(10))
      } else {
        prop = elfCoords
      }
      
      if (proposals.contains(prop)) {
        proposals(prop) += 1
      } else {
        proposals(prop) = 1
      }
      
      proposal(elfCoords) = prop
    }
    
    for ((elf, prop) <- proposal) {
      if (proposals(prop) == 1) {
        grid(elf) = false
        grid(prop) = true
        if (elf != prop) {
          continue = true
        }
      }
    }
    
    q = rest ++ firstThree
    rounds += 1
    
    if (rounds == 10) {
      for ((x, y) <- grid) {
        minX = minX min x
        maxX = maxX max x
        minY = minY min y
        maxY = maxY max y
      }
      
      val width = maxX - minX + 1
      val height = maxY - minY + 1
      val empty = width * height - grid.size
      
      println("Empty tiles after 10 rounds: " + empty)
    }
  }
  
  def visualize() = {
    var res = ""
    for (y <- minY to maxY) {
      for (x <- minX to maxX) {
        if (grid((x, y))) {
          res += '#'
        } else {
          res += '.'
        }
      }
      res += "\n"
    }
    println(res.count( _ == '.' ))
    println(res)
  }
  //visualize()
  
  println("First round when no elf moves: " + rounds)
}