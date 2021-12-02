

package day17

object ConwayCubes extends App {
  val input = """..#..#...###..#.#..##.#.#.#.#.#..#..###......#..#...######....#."""
  val cycles = 6
  val xLen = 8
  val yLen = 8
  var activeCubes = Array[(Int,Int,Int)]()
  for (x <- 0 until xLen; y <- 0 until yLen) {
    val c = input(y * xLen + x)
    c match {
      case '#' => activeCubes = activeCubes :+ ((x,y,0))
      case _ => {}
    }
  }
  def neighbours(coord: (Int,Int,Int)): Array[(Int,Int,Int)] = {
    (for (x <- coord._1 - 1 to coord._1 + 1; y <- coord._2 - 1 to coord._2 + 1; z <- coord._3 - 1 to coord._3 + 1; if(x != coord._1 || y != coord._2 || z != coord._3)) yield (x,y,z)).toArray
  }
  def area: (Int,Int,Int,Int,Int,Int) = {
    var xMin, yMin, zMin = Int.MaxValue; var xMax, yMax, zMax = Int.MinValue
    var i = 0
    while (i < activeCubes.length) {
      val (x, y, z) = activeCubes(i)
      xMin = xMin min x; xMax = xMax max x
      yMin = yMin min y; yMax = yMax max y
      zMin = zMin min z; zMax = zMax max z
      i += 1
    }
    (xMin-1,xMax+1,yMin-1,yMax+1,zMin-1,zMax+1)
  }
  
  var i = 0
  while (i < cycles) {
    val (xMin,xMax,yMin,yMax,zMin,zMax) = area
    val next = scala.collection.mutable.ArrayBuffer[(Int,Int,Int)]()
    for (x <- xMin to xMax; y <- yMin to yMax; z <- zMin to zMax) {
      val coord = (x,y,z)
      val neigh = neighbours(coord)
      val count = neigh.count( activeCubes.contains(_) )
      if (activeCubes.contains(coord)) {
        if (count == 2 || count == 3) next += coord
      } else {
        if (count == 3) next += coord
      }
    }
    activeCubes = next.toArray
    i += 1
  }
  println("Active cubes after six iterations: " + activeCubes.length)
  
  var activeCubes4D = Array[(Int,Int,Int,Int)]()
  for (x <- 0 until xLen; y <- 0 until yLen) {
    val c = input(y * xLen + x)
    c match {
      case '#' => activeCubes4D = activeCubes4D :+ ((x,y,0,0))
      case _ => {}
    }
  }
  def neighbours(coord: (Int,Int,Int,Int)): Array[(Int,Int,Int,Int)] = {
    (for (x <- coord._1 - 1 to coord._1 + 1; y <- coord._2 - 1 to coord._2 + 1; z <- coord._3 - 1 to coord._3 + 1; w <- coord._4 - 1 to coord._4 + 1; if(x != coord._1 || y != coord._2 || z != coord._3 || w != coord._4)) yield (x,y,z,w)).toArray
  }
  def area4D: (Int,Int,Int,Int,Int,Int,Int,Int) = {
    var xMin, yMin, zMin, wMin = Int.MaxValue; var xMax, yMax, zMax, wMax = Int.MinValue
    var i = 0
    while (i < activeCubes4D.length) {
      val (x, y, z, w) = activeCubes4D(i)
      xMin = xMin min x; xMax = xMax max x
      yMin = yMin min y; yMax = yMax max y
      zMin = zMin min z; zMax = zMax max z
      wMin = wMin min w; wMax = wMax max w
      i += 1
    }
    (xMin-1,xMax+1,yMin-1,yMax+1,zMin-1,zMax+1,wMin-1,wMax+1)
  }
  
  i = 0
  while (i < cycles) {
    val (xMin,xMax,yMin,yMax,zMin,zMax,wMin,wMax) = area4D
    val next = scala.collection.mutable.ArrayBuffer[(Int,Int,Int,Int)]()
    for (x <- xMin to xMax; y <- yMin to yMax; z <- zMin to zMax; w <- wMin to wMax) {
      val coord = (x,y,z,w)
      val neigh = neighbours(coord)
      val count = neigh.count( activeCubes4D.contains(_) )
      if (activeCubes4D.contains(coord)) {
        if (count == 2 || count == 3) next += coord
      } else {
        if (count == 3) next += coord
      }
    }
    activeCubes4D = next.toArray
    i += 1
  }
  println("Active cubes after six iterations in 4D: " + activeCubes4D.length)
}