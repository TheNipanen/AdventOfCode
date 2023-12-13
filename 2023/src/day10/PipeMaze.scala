package day10

import scala.io.Source
import scala.collection.mutable.{ ArrayBuffer, HashMap }

object PipeMaze extends App {
  val s = Source.fromFile("src/day10/input.txt")
  val lines = try s.getLines().toArray finally s.close()

  val neighbors = HashMap[(Int, Int), ((Int, Int), (Int, Int))]()

  var i = 0
  val h = lines.length
  val w = lines(0).length
  var start: (Int, Int) = null
  while (i < h) {
    val line = lines(i)
    var j = 0
    while (j < w) {
      val c = line(j)
      c match {
        case '|' => neighbors((i, j)) = ((i - 1, j), (i + 1, j))
        case '-' => neighbors((i, j)) = ((i, j - 1), (i, j + 1))
        case 'L' => neighbors((i, j)) = ((i - 1, j), (i, j + 1))
        case 'J' => neighbors((i, j)) = ((i - 1, j), (i, j - 1))
        case '7' => neighbors((i, j)) = ((i, j - 1), (i + 1, j))
        case 'F' => neighbors((i, j)) = ((i + 1, j), (i, j + 1))
        case 'S' => start = (i, j)
        case _   => {}
      }
      j += 1
    }
    i += 1
  }

  def connectsToStart(coords: (Int, Int)): Boolean = {
    val n = neighbors.getOrElse(coords, ((-1, -1), (-1, -1)))
    n._1 == start || n._2 == start
  }

  val startNeighbors = Array((start._1 + 1, start._2), (start._1 - 1, start._2), (start._1, start._2 + 1), (start._1, start._2 - 1))
    .filter(connectsToStart(_))
  if (startNeighbors.length != 2) {
    println("Error")
  }
  neighbors(start) = (startNeighbors(0), startNeighbors(1))

  var current = neighbors(start)._1
  var prev = start
  val steps = HashMap[(Int, Int), Int](((start, 0)))
  var currentSteps = 1
  
  val boundary = ArrayBuffer[(Int, Int)](start)
  
  def loop() = {
    while (current != start) {
      boundary += current
      val prevSteps = steps.getOrElse(current, Int.MaxValue)
      steps(current) = currentSteps min prevSteps
      currentSteps += 1
      val n = neighbors(current)
      val temp = current
      current = if (n._1 != prev) n._1 else n._2
      prev = temp
    }
  }
  
  loop()
  val boundaryPoints = boundary.toArray
  
  current = neighbors(start)._2
  prev = start
  currentSteps = 1
  loop()
  
  val farthest = steps.values.max
  println("Steps from start to farthest position: " + farthest)
  
  // Calc area with Shoelace trapezoid formula
  var sum = 0.0
  val n = boundaryPoints.length
  i = 1
  while (i <= n) {
    val ind = i - 1
    val next = if (i == n) 0 else i
    val first = boundaryPoints(ind)
    val second = boundaryPoints(next)
    sum += (first._1 + second._1) * (first._2 - second._2)
    i += 1
  }
  val area = math.abs(sum / 2)
  
  // Amount of inside points with Pick's theorem
  val inside = area - n / 2 + 1
  println("Tiles enclosed by the loop: " + inside.toInt)
}