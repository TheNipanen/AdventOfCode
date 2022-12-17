package day17

import scala.io.Source
import scala.collection.mutable.{ ArrayBuffer, HashSet }

object PyroclasticFlow extends App {
  val s = Source.fromFile("src/day17/input.txt")
  val jetPattern = try s.getLines().toArray.flatMap( _.toCharArray() ) finally s.close()
  
  var rocksFallen = 0
  var iPattern = 0
  val lPattern = jetPattern.length
  
  val rocks = Array(
      Array((0,0),(1,0),(2,0),(3,0)), 
      Array((1,0),(0,1),(1,1),(2,1),(1,2)), 
      Array((0,0),(1,0),(2,0),(2,1),(2,2)), 
      Array((0,0),(0,1),(0,2),(0,3)), 
      Array((0,0),(1,0),(0,1),(1,1))
  )
  var iRock = 0
  val lRock = rocks.length
  
  val grid = HashSet[(Int, Int)]()
  val floorY = 0
  val leftWallX = 0
  val rightWallX = 6
  
  var maxY = -1
  
  val dir = Map('<' -> (-1,0), '>' -> (1,0))
  
  def isOccupied(pos: (Int, Int)): Boolean = {
    pos._1 < 0 || pos._1 > 6 || pos._2 < 0 || grid(pos)
  }
  def isOccupied(pos: (Int, Int), rock: Array[(Int, Int)]): Boolean = {
    rock.exists( r => { val p = (pos._1 + r._1, pos._2 + r._2); isOccupied(p) } )
  }
  
  def moveDir(rockPos: (Int, Int), dir: (Int, Int)) = {
    val newPos = (rockPos._1 + dir._1, rockPos._2 + dir._2)
    val rock = rocks(iRock)
    if (isOccupied(newPos, rock)) {
      rockPos
    } else {
      newPos
    }
  }
  
  val heights = ArrayBuffer[(Int, Int, Long)]()
  
  while (rocksFallen < 10000) {
    // Left bottom corner of falling rock
    var rockPos = (2, maxY + 4)
    var continue = true
    val ip = iPattern
    while (continue) {
      rockPos = moveDir(rockPos, dir(jetPattern(iPattern)))
      val fallen = moveDir(rockPos, (0, -1))
      continue = fallen != rockPos
      rockPos = fallen
      iPattern = (iPattern + 1) % lPattern
    }
    val prev = maxY
    rocks(iRock).foreach( r => { val p = (rockPos._1 + r._1, rockPos._2 + r._2); grid(p) = true; maxY = maxY max p._2 } )
    rocksFallen += 1
    heights += ((iRock, ip, maxY - prev))
    iRock = (iRock + 1) % lRock
    if (rocksFallen == 2022) {
      val height = maxY + 1
      println("Tower height after 2022 rocks have fallen: " + height)
    }
  }
  
  // Floyd's cycle-finding algorithm
  def floyd() = {
    var tortoiseIdx = 1
    var hareIdx = 2
    //println("Starting...")
    while (heights(tortoiseIdx) != heights(hareIdx)) {
      tortoiseIdx += 1
      hareIdx += 2
    }
    //println("Found: " + tortoiseIdx + "," + hareIdx)
    
    var mu = 0
    tortoiseIdx = 0
    while (heights(tortoiseIdx) != heights(hareIdx)) {
      tortoiseIdx += 1
      hareIdx += 1
      mu += 1
    }
    //println("Found mu: " + mu)
    
    var lam = 1
    hareIdx = tortoiseIdx + 1
    while (heights(tortoiseIdx) != heights(hareIdx)) {
      hareIdx += 1
      lam += 1
    }
    //println("Found lambda: " + lam)
    
    (lam, mu)
  }
  
  val (cycleLength, cyclePos) = floyd()
  val cycle = heights.drop(cyclePos).take(cycleLength).toArray
  val beginning = heights.take(cyclePos)
  
  var height = -1L
  height += beginning.map( _._3 ).sum
  
  var rocksLeft = (1000000000000L - cyclePos.toLong)
  val cycleRepeated = rocksLeft / cycleLength.toLong
  val cycleSum = cycle.map( _._3 ).sum
  height += cycleSum * cycleRepeated
  
  rocksLeft -= cycleRepeated * cycleLength.toLong
  height += cycle.take(rocksLeft.toInt).map( _._3 ).sum
  
  val h = height + 1L
  println("Tower height after 1000000000000 rocks have fallen: " + h)
}