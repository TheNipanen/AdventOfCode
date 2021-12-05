package day12

import scala.io.Source

object RainRisk extends App {
  val s = Source.fromFile("src/day12/input.txt")
  val reader = s.bufferedReader()
  
  var x = 0
  var y = 0
  var orientation = 'E'
  def move(dir: Char, dist: Int) = {
    dir match {
      case 'E' => x += dist
      case 'W' => x -= dist
      case 'N' => y += dist
      case 'S' => y -= dist
    }
  }
  def turn(dir: Char, d: Int) = {
    var deg = d
    while (deg > 0) {
      dir match {
        case 'L' => {
          orientation = orientation match {
            case 'E' => 'N'
            case 'W' => 'S'
            case 'N' => 'W'
            case 'S' => 'E'
          }
        }
        case 'R' => {
          orientation = orientation match {
            case 'E' => 'S'
            case 'W' => 'N'
            case 'N' => 'E'
            case 'S' => 'W'
          }
        }
      }
      deg -= 90
    }
  }
  def parse(line: String): (Char, Int) = {
    val (dir, d) = line.splitAt(1)
    return (dir.charAt(0), d.toInt)
  }
  
  var line = reader.readLine()
  while (line != null) {
    val (dir, d) = parse(line)
    dir match {
      case 'F' => move(orientation, d)
      case 'L' | 'R' => turn(dir, d)
      case _ => move(dir, d)
    }
    line = reader.readLine()
  }
  s.close()
  val dist = math.abs(y) + math.abs(x)
  println("Mahattan distance: " + dist)
  
  x = 0
  y = 0
  var wpX = 10
  var wpY = 1
  def moveWP(dir: Char, dist: Int) = {
    dir match {
      case 'E' => wpX += dist
      case 'W' => wpX -= dist
      case 'N' => wpY += dist
      case 'S' => wpY -= dist
    }
  }
  def moveShip(times: Int) = {
    var t = times
    while (t > 0) {
      x += wpX
      y += wpY
      t -= 1
    }
  }
  def rotateWP(dir: Char, deg: Int) = {
    var d = deg
    while (d > 0) {
      dir match {
        case 'L' => {
          val temp = wpX
          wpX = -wpY
          wpY = temp
        }
        case 'R' => {
          val temp = wpX
          wpX = wpY
          wpY = -temp
        }
      }
      d -= 90
    }
  }
  
  val ss = Source.fromFile("src/day12/input.txt")
  val r = ss.bufferedReader()
  line = r.readLine()
  while (line != null) {
    val (dir, d) = parse(line)
    dir match {
      case 'F' => moveShip(d)
      case 'L' | 'R' => rotateWP(dir, d)
      case _ => moveWP(dir, d)
    }
    line = r.readLine()
  }
  ss.close()
  val dist2 = math.abs(y) + math.abs(x)
  println("Mahattan distance again: " + dist2)
}