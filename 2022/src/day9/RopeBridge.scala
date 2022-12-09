package day9

import scala.io.Source
import scala.collection.mutable.HashSet
import scala.math.{ abs, min }

object RopeBridge extends App {
  val s = Source.fromFile("src/day9/input.txt")
  val lines = //Array("R 5", "U 8", "L 8", "D 3", "R 17", "D 10", "L 25", "U 20")
  try s.getLines().toArray finally s.close()
  
  val visited = HashSet[(Int, Int)]((0,0))
  val visited2 = HashSet[(Int, Int)]((0,0))
  var h = (0,0)
  var t = (0,0)
  val positions = Array.fill(10)((0,0))
  
  def neighbors(pos: (Int, Int)) = {
    Array((pos._1 + 1, pos._2), (pos._1 - 1, pos._2), (pos._1, pos._2 + 1), (pos._1, pos._2 - 1))
  }
  
  def moveHead(di: Int, dj: Int) = {
    val prevH = h
    h = (h._1 + di, h._2 + dj)
    if (abs(h._1 - t._1) > 1 || abs(h._2 - t._2) > 1) {
      t = prevH
      visited(t) = true
    }
    
    var prevP = positions(0)
    var prevMovedNonDiagonally = true
    var prevDiagonalMove: Option[(Int, Int)] = None
    positions(0) = (positions(0)._1 + di, positions(0)._2 + dj)
    for (j <- 1 until positions.length) {
      val hPos = positions(j-1)
      val cPos = positions(j)
      val moveNeeded = abs(hPos._1 - cPos._1) > 1 || abs(hPos._2 - cPos._2) > 1
      val diagonalDiff = abs(hPos._1 - cPos._1) > 0 && abs(hPos._2 - cPos._2) > 0
      if (moveNeeded && prevMovedNonDiagonally) {//(abs(hPos._1 - cPos._1) > 1 && !(abs(hPos._2 - cPos._2) > 1)) || (abs(hPos._2 - cPos._2) > 1 && !(abs(hPos._1 - cPos._1) > 1))) {
        positions(j) = prevP
      } else if (moveNeeded && diagonalDiff) {
        val dm = prevDiagonalMove.get
        val next = (cPos._1 + dm._1, cPos._2 + dm._2)
        positions(j) = next
      } else if (moveNeeded) {
        val n = neighbors(hPos).sortBy( nPos => {
          val a1 = abs(nPos._1 - cPos._1);
          val a2 = abs(nPos. _2 - cPos._2);
          (min(a1, a2), a1 + a2)
        })
        positions(j) = n(0)
      }
      prevP = cPos
      prevMovedNonDiagonally = positions(j)._1 == cPos._1 || positions(j)._2 == cPos._2
      if (prevMovedNonDiagonally) {
        prevDiagonalMove = None
      } else {
        prevDiagonalMove = Some((positions(j)._1 - cPos._1, positions(j)._2 - cPos._2))
      }
      if (j == positions.length - 1) {
        visited2(positions(j)) = true
      }
    }
  }
  
  def visualize() = {
    var res = ""
    for (i <- -15 to 5) {
      for (j <- -11 to 14) {
        val ind = positions.indexOf((i, j))
        if (ind != -1) {
          if (ind == 0) {
            res += "H"
          } else {
            res += ind
          }
        } else if (i == 0 && j == 0) {
          res += "s"
        } else {
          res += "."
        }
      }
      res += "\n"
    }
    println(res)
  }
  
  var i = 0
  //visualize()
  while (i < lines.length) {
    val parts = lines(i).split(" ")
    val amount = parts(1).toInt
    var ii = 0
    while (ii < amount) {
      parts(0) match {
        case "D" => {
          moveHead(1, 0)
        }
        case "U" => {
          moveHead(-1, 0)
        }
        case "R" => {
          moveHead(0, 1)
        }
        case "L" => {
          moveHead(0, -1)
        }
      }
      ii += 1
      //visualize()
    }
    i += 1
    //visualize()
  }
  
  println("Positions visited: " + visited.size)
  println("Positions visited by long tail: " + visited2.size)
}