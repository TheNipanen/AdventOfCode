package day18

import scala.io.Source
import scala.collection.mutable.{ HashSet, Stack }

object BoilingBoulders extends App {
  val s = Source.fromFile("src/day18/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  // Calculate these so we know the bounds of the grid for dfs
  var maxX = 0
  var maxY = 0
  var maxZ = 0
  
  // Parse input and save the cubes in a HashSet
  val cubes = HashSet[(Int, Int, Int)]()
  var i = 0
  while (i < lines.length) {
    val c = lines(i).split(",").map( _.toInt )
    val cube = (c(0), c(1), c(2))
    cubes(cube) = true
    maxX = maxX max cube._1
    maxY = maxY max cube._2
    maxZ = maxZ max cube._3
    i += 1
  }
  
  // Check if coordinate is in bounds. Used in the DFS so we don't have an infinite grid to search
  def isInBounds(coord: (Int, Int, Int)) = {
    coord._1 >= -1 && coord._1 <= maxX + 1 && coord._2 >= -1 && coord._2 <= maxY + 1 && coord._3 >= -1 && coord._3 <= maxZ + 1
  }
  
  // Get neighbors of the cube. If in DFS, we only want air cubes
  def neighbors(cube: (Int, Int, Int), dfs: Boolean) = {
    Array(
        (cube._1 + 1, cube._2, cube._3), 
        (cube._1 - 1, cube._2, cube._3), 
        (cube._1, cube._2 + 1, cube._3), 
        (cube._1, cube._2 - 1, cube._3), 
        (cube._1, cube._2, cube._3 + 1), 
        (cube._1, cube._2, cube._3 - 1)
    ).filter( c => !dfs || (isInBounds(c) && !cubes(cube)) )
  }
  
  val visited = HashSet[(Int, Int, Int)]((-1,-1,-1))
  
  class Frame(cube: (Int, Int, Int)) {
    val n = neighbors(cube, true).iterator
  }
  
  // Non-recursive DFS, so we don't get stack overflow
  val stack = Stack[Frame](new Frame((-1,-1,-1)))
  while (stack.nonEmpty) {
    val f = stack.top
    if (f.n.hasNext) {
      val neighbor = f.n.next
      if (!visited(neighbor)) {
        visited(neighbor) = true
        stack.push(new Frame(neighbor))
      }
    } else {
      stack.pop()
    }
  }
  
  def reachable(cube: (Int, Int, Int)) = {
    visited(cube)
  }
  
  var surface = 0
  var extSurface = 0
  for (cube <- cubes) {
    for (n <- neighbors(cube, false)) {
      if (!cubes(n)) {
        surface += 1
        // If the air cube can be reached from outside, it's a part of the exterior surface area
        if (reachable(n)) {
          extSurface += 1
        }
      }
    }
  }
  
  println("Surface area of the droplet: " + surface)
  println("Exterior surface area of the droplet: " + extSurface)
}