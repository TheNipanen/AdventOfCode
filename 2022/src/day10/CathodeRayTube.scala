package day10

import scala.io.Source

object CathodeRayTube extends App {
  val s = Source.fromFile("src/day10/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val interestingCycles = Array(20, 60, 100, 140, 180, 220)
  val screen = Array.fill(240)('.')
  
  def drawPixel() = {
    val idx = cycles - 1
    val position = idx % 40
    if (position >= x - 1 && position <= x + 1) {
      screen(idx) = '#'
    }
  }
  
  def checkCycle() = {
    if (interestingCycles.contains(cycles)) {
      result += x * cycles
    }
  }
  
  def command(line: String) = {
    cycles += 1
    checkCycle()
    drawPixel()
    val parts = line.split(" ")
    if (parts(0) == "addx") {
      cycles += 1
      checkCycle()
      drawPixel()
      x += parts(1).toInt
    }
  }
  
  var result = 0
  var i = 0
  var cycles = 0
  var x = 1
  while (i < lines.length) {
    val line = lines(i)
    command(line)
    i += 1
  }
  
  println("Sum of signal strengths: " + result)
  val render = screen.grouped(40).toArray.map( _.foldLeft("")( _ + _ ) ).reduceLeft( _ + "\n" + _ )
  println(render)
}