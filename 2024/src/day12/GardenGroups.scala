package day12

import scala.io.Source
import scala.collection.mutable.{HashMap, HashSet}

// Use any union find data structure
import lib.UnionFind

object GardenGroups extends App {
  val s = Source.fromFile("src/day12/input.txt")
  val garden = try s.getLines().toArray finally s.close()
  
  val sets = new UnionFind[(Int, Int)]()
  
  val yLen = garden.length
  val xLen = garden(0).length()
  
  var y = 0
  while (y < yLen) {
    val line = garden(y)
    var x = 0
    while (x < xLen) {
      sets.makeSet((y, x))
      
      val c = line(x)
      for ((neighborY, neighborX) <- Vector((y, x-1), (y-1, x))) {
        if (neighborY >= 0 && neighborY < yLen && neighborX >= 0 && neighborX < xLen && garden(neighborY)(neighborX) == c) {
          sets.union((neighborY, neighborX), (y, x))
        }
      }
      
      x += 1
    }
    y += 1
  }
  
  def isCornerInDirection(side1: (Int, Int), side2: (Int, Int), corner: (Int, Int), group: HashSet[(Int, Int)]): Boolean = {
    (!group(side1) && !group(side2)) ||
    (!group(corner) && group(side1) && group(side2))
  }
  
  def nOfCorners(coords: (Int, Int), group: HashSet[(Int, Int)]): Int = {
    val (y, x) = coords
    var sum = 0
    val topSide = (y-1, x)
    val rightSide = (y, x+1)
    val botSide = (y+1, x)
    val leftSide = (y, x-1)
    val topLeft = (y-1, x-1)
    val topRight = (y-1, x+1)
    val botRight = (y+1, x+1)
    val botLeft = (y+1, x-1)
    
    if (isCornerInDirection(topSide, leftSide, topLeft, group)) sum += 1
    if (isCornerInDirection(topSide, rightSide, topRight, group)) sum += 1
    if (isCornerInDirection(botSide, leftSide, botLeft, group)) sum += 1
    if (isCornerInDirection(botSide, rightSide, botRight, group)) sum += 1
    
    sum
  }
  
  def getSides(group: HashSet[(Int, Int)]): Int = {
    // Equally many sides and corners
    group.toVector.map( nOfCorners(_, group) ).sum
  }
  
  var price = 0L
  var bulkPrice = 0L
  for (group <- sets.getSets()) {
    val area = group.size.toLong
    val perimeter = group.toVector.map( elem => Vector((elem._1 - 1, elem._2), (elem._1 + 1, elem._2), (elem._1, elem._2 - 1), (elem._1, elem._2 + 1)).count( !group(_) ) ).sum
    val sides = getSides(group)
    price += area * perimeter.toLong
    bulkPrice += area * sides.toLong
  }
  
  println(price)
  println(bulkPrice)
}