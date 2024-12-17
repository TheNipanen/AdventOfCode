package day17

import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.math.pow

object ChronospatialComputer extends App {
  val s = Source.fromFile("src/day17/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val origA = lines(0).dropWhile( !_.isDigit ).toLong
  val origB = lines(1).dropWhile( !_.isDigit ).toLong
  val origC = lines(2).dropWhile( !_.isDigit ).toLong
  
  var A = origA
  var B = origB
  var C = origC
  
  val program = lines(4).dropWhile( !_.isDigit ).split(",").map( _.toInt )
  
  def runProgram(): ArrayBuffer[Int] = {
    val output = ArrayBuffer[Int]()
    
    def combo(operand: Int): Long = {
      operand match {
        case 0 => 0L
        case 1 => 1L
        case 2 => 2L
        case 3 => 3L
        case 4 => A
        case 5 => B
        case 6 => C
        case _ => throw new Exception("wrong combo")
      }
    }
    
    var i = 0
    
    def runCommand(ins: Int, operand: Int) = {
      ins match {
        case 0 => {
          A /= pow(2, combo(operand)).toLong
        }
        case 1 => {
          B ^= operand
        }
        case 2 => {
          B = combo(operand) % 8
        }
        case 3 => {
          if (A != 0) {
            i = operand
          }
        }
        case 4 => {
          B ^= C
        }
        case 5 => {
          output += (combo(operand) % 8L).toInt
        }
        case 6 => {
          B = A / pow(2, combo(operand)).toInt
        }
        case 7 => {
          try {
          C = A / pow(2, combo(operand)).toInt
          } catch {
            case e: Exception => {
              println(initial)
              throw e
            }
          }
        }
      }
    }
    
    while (i < program.length - 1) {
      val tmp = i
      runCommand(program(i), program(i+1))
      if (i == tmp) {
        i += 2
      }
    }
    output
  }
  
  println(runProgram().mkString(","))
  
  var initial = 1L
  var increment = 1L
  var found = false
  while (!found) {
    A = initial
    B = origB
    C = origC
    val out = runProgram().toArray
    
    // Output depends on bit operations
    // We can fix at least the first (k - 3) * 3 bits of INITIAL, where k is the number of elements from start of out tha match program
    val matchingPrefixLen = out.zipWithIndex.prefixLength( pair => pair._1 == program(pair._2) )
    increment = increment max (1 << (0 max ((matchingPrefixLen - 3) * 3)))
    if (out.sameElements(program)) {
      found = true
    } else {
      initial += increment
    }
  }
  println(initial)
}