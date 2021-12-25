package day25

import scala.io.Source
import scala.collection.mutable.HashSet

object SeaCucumber extends App {
  val s = Source.fromFile("src/day25/input.txt")
  var grid = s.getLines().toArray.map( _.split("").map( s => if (s == ">") 1 else if (s == "v") -1 else 0 ) )
  s.close()
  
  //var east = HashSet[(Int,Int)]()
  //var south = HashSet[(Int,Int)]()
  
  val rows = grid.size
  val cols = grid(0).size
//  for (r <- 0 until rows) {
//    for (c <- 0 until cols) {
//      print(grid(r)(c) match {case 1 => '>' case -1 => 'v' case 0 => '.'})
//    }
//    println()
//  }
  
  //(0 until rows).foreach( r => (0 until cols).foreach( c => if (grid(r)(c) == 1) east((r,c)) = true ) )
  //(0 until rows).foreach( r => (0 until cols).foreach( c => if (grid(r)(c) == -1) south((r,c)) = true ) )
  
  def nextRow(r: Int) = {
    if (r + 1 == rows) 0
    else r + 1
  }
  def nextCol(c: Int) = {
    if (c + 1 == cols) 0
    else c + 1
  }
  def getNextG(): (Array[Array[Int]], Int) = {
    val nextG = grid.clone().map( _.clone() )
    var moved = 0
    for (r <- 0 until rows; c <- 0 until cols) {
      if (grid(r)(c) == 1) {
        val nc = nextCol(c)
        if (grid(r)(nc) == 0) {
          nextG(r)(c) = 0
          nextG(r)(nc) = 1
          moved += 1
        }
      }
    }
    val nextNextG = nextG.clone().map( _.clone() )
    for (r <- 0 until rows; c <- 0 until cols) {
      if (nextG(r)(c) == -1) {
        val nr = nextRow(r)
        if (nextG(nr)(c) == 0) {
          nextNextG(r)(c) = 0
          nextNextG(nr)(c) = -1
          moved += 1
        }
      }
    }
    (nextNextG, moved)
  }
//  def getNextE() = {
//    val nextE = HashSet[(Int,Int)]()
//    for ((r,c) <- east) {
//      val nc = nextCol(c)
//      if (!east((r,nc)) && !south((r,nc))) {
//        nextE((r,nc)) = true
//      }
//    }
//    nextE
//  }
//  def getNextS(e: HashSet[(Int,Int)]) = {
//    val nextS = HashSet[(Int,Int)]()
//    for ((r,c) <- south) {
//      val nr = nextRow(r)
//      if (!e((nr,c)) && !south((nr,c))) {
//        nextS((nr,c)) = true
//      }
//    }
//    nextS
//  }
//  def equal(nEast: HashSet[(Int,Int)], nSouth: HashSet[(Int,Int)]) = {
//    east.forall( nEast(_) ) && nEast.forall( east(_) ) && south.forall( nSouth(_) ) && nSouth.forall( south(_) )
//  }
  
  var steps = 0L
  var finished = false
  while (!finished) {
    val (nextGrid, moved) = getNextG()
    finished = moved == 0
    //println(moved)
    grid = nextGrid
    steps += 1
  }
  
  println("The first step after no sea cucumbers move: " + steps)
}