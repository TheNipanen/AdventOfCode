package day7
import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, ListBuffer}

object Laboratories extends App {
  val s = Source.fromFile("src/day7/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  var splits = 0
  
  val beamCoordsX = ArrayBuffer[HashSet[Int]](HashSet(lines(0).indexOf('S')))
  var yCoord = 1
  
  def printCoordLine(yCoord: Int) = {
    val line = lines(yCoord)
    var i = 0
    while (i < line.size) {
      if (beamCoordsX(yCoord - 1)(i)) {
        print('|')
      } else {
        print(line(i))
      }
      i += 1
    }
    println()
  }
  
  val timelines = HashMap[(Int, Int), Long]((0, lines(0).indexOf('S')) -> 1L)
  
  while (yCoord < lines.size) {
    val line = lines(yCoord)
    val nextCoords = HashSet[Int]()
    for (coordX <- beamCoordsX(yCoord - 1)) {
      if (line(coordX) == '^') {
        nextCoords(coordX - 1) = true
        nextCoords(coordX + 1) = true
        val current1 = timelines.getOrElse((yCoord, coordX - 1), 0L)
        timelines((yCoord, coordX - 1)) = current1 + timelines((yCoord - 1, coordX))
        val current2 = timelines.getOrElse((yCoord, coordX + 1), 0L)
        timelines((yCoord, coordX + 1)) = current2 + timelines((yCoord - 1, coordX))
        splits += 1
      } else {
        nextCoords(coordX) = true
        val current = timelines.getOrElse((yCoord, coordX), 0L)
        timelines((yCoord, coordX)) = current + timelines((yCoord - 1, coordX))
      }
    }
    beamCoordsX += nextCoords
    //printCoordLine(yCoord)
    yCoord += 1
  }
  
  println(splits)
  
  val totalTimelines = timelines.filter({ case ((y, x), _amount) => y == beamCoordsX.size - 1 }).map({ case ((y, x), amount) => amount }).sum
  println(totalTimelines)
}