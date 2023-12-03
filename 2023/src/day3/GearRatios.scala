package day3

import scala.io.Source
import scala.collection.mutable.ListBuffer

object GearRatios extends App {  
  val s = Source.fromFile("src/day3/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  def adjacentToSymbol(i: Int, j: Int): Boolean = {
    Array((i-1,j-1), (i-1, j), (i-1, j+1), (i, j-1), (i, j+1), (i+1, j-1), (i+1, j), (i+1, j+1))
      .filter( pair => pair._1 >= 0 && pair._1 < h && pair._2 >= 0 && pair._2 < w )
      .exists( pair => !lines(pair._1)(pair._2).isDigit && lines(pair._1)(pair._2) != '.' )
  }
  def topOrBot(i: Int, j: Int): ListBuffer[Int] = {
    val result = ListBuffer[Int]()
    val left = i >= 0 && i < h && j-1 >= 0 && lines(i)(j-1).isDigit
    val right = i >= 0 && i < h && j+1 < w && lines(i)(j+1).isDigit
    val center = i >= 0 && i < h && lines(i)(j).isDigit
    if (left && right && !center) {
      var number = ""
      var jj = j - 1
      while (jj >= 0 && lines(i)(jj).isDigit) {
        number += lines(i)(jj)
        jj -= 1
      }
      result += number.reverse.toInt
      
      number = ""
      jj = j + 1
      while (jj < w && lines(i)(jj).isDigit) {
        number += lines(i)(jj)
        jj += 1
      }
      result += number.toInt
    }
    if (center) {
      var number = ""
      var jj = j
      number += lines(i)(jj)
      if (right) {
        jj = j + 1
        while (jj < w && lines(i)(jj).isDigit) {
          number += lines(i)(jj)
          jj += 1
        }
      }
      if (left) {
        number = number.reverse
        jj = j - 1
        while (jj >= 0 && lines(i)(jj).isDigit) {
          number += lines(i)(jj)
          jj -= 1
        }
        result += number.reverse.toInt
      } else {
        result += number.toInt
      }
    }
    if (left && !center && !right) {
      var number = ""
      var jj = j - 1
      while (jj >= 0 && lines(i)(jj).isDigit) {
        number += lines(i)(jj)
        jj -= 1
      }
      result += number.reverse.toInt
    }
    if (right && !center && ! left) {
      var number = ""
      var jj = j + 1
      while (jj < w && lines(i)(jj).isDigit) {
        number += lines(i)(jj)
        jj += 1
      }
      result += number.toInt
    }
    
    result
  }
  def adjacentPartNumbers(i: Int, j: Int): Array[Int] = {
    val result = ListBuffer[Int]()
    if (j-1 >= 0 && lines(i)(j-1).isDigit) {
      var number = ""
      var jj = j - 1
      while (jj >= 0 && lines(i)(jj).isDigit) {
        number += lines(i)(jj)
        jj -= 1
      }
      result += number.reverse.toInt
    }
    if (j+1 < w && lines(i)(j+1).isDigit) {
      var number = ""
      var jj = j + 1
      while (jj < w && lines(i)(jj).isDigit) {
        number += lines(i)(jj)
        jj += 1
      }
      result += number.toInt
    }
    
    result ++= topOrBot(i-1, j)
    result ++= topOrBot(i+1, j)
    
    result.toArray
  }
  
  var sum = 0
  var sumGear = 0
  
  val h = lines.length
  val w = lines(0).length
  var i = 0
  while (i < h) {
    val line = lines(i)
    var j = 0
    while (j < w) {
      var c = line(j)
      if (c.isDigit) {
        var number = ""
        var adjacent = false
        while (j < w && line(j).isDigit) {
          c = line(j)
          number += c
          if (adjacentToSymbol(i, j)) {
            adjacent = true
          }
          j += 1
        }
        if (adjacent) {
          sum += number.toInt
        }
      } else if (c == '*') {
        val numbers = adjacentPartNumbers(i, j)
        if (numbers.length == 2) {
          val gearRatio = numbers(0) * numbers(1)
          sumGear += gearRatio
        }
        j += 1
      } else {
        j += 1
      }
    }
    i += 1
  }
  
  println("Sum of part numbers: " + sum)
  println("Sum of gear ratios: " + sumGear)
}