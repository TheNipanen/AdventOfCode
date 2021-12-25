package day24

import scala.io.Source

object ArithmeticLogicUnit extends App {
  val s = Source.fromFile("src/day24/input.txt")
  val instructions = s.getLines().toArray.map( ins => {val i = ins.split(" "); (i(0), i.drop(1))} )
  s.close()
//  var i = 0
//  val variables = Array(0,0,0,0)
//  val index = Map[String,Int]("w" -> 0, "x" -> 1, "y" -> 2, "z" -> 3)
//  
//  def process(ins: String, targets: Array[String], input: String) = {
//    ins match {
//      case "inp" => {
//        val a = targets(0)
//        variables(index(a)) = input(i).asDigit
//        i += 1
//      }
//      case "add" => {
//        val a = targets(0)
//        val bs = targets(1)
//        val b = if (bs == "w" || bs == "x" || bs == "y" || bs == "z") variables(index(bs)) else bs.toInt
//        variables(index(a)) += b
//      }
//      case "mul" => {
//        val a = targets(0)
//        val bs = targets(1)
//        val b = if (bs == "w" || bs == "x" || bs == "y" || bs == "z") variables(index(bs)) else bs.toInt
//        variables(index(a)) *= b
//      }
//      case "div" => {
//        val a = targets(0)
//        val bs = targets(1)
//        val b = if (bs == "w" || bs == "x" || bs == "y" || bs == "z") variables(index(bs)) else bs.toInt
//        variables(index(a)) /= b
//      }
//      case "mod" => {
//        val a = targets(0)
//        val bs = targets(1)
//        val b = if (bs == "w" || bs == "x" || bs == "y" || bs == "z") variables(index(bs)) else bs.toInt
//        variables(index(a)) %= b
//      }
//      case "eql" => {
//        val a = targets(0)
//        val bs = targets(1)
//        val b = if (bs == "w" || bs == "x" || bs == "y" || bs == "z") variables(index(bs)) else bs.toInt
//        variables(index(a)) = if (variables(index(a)) == b) 1 else 0
//      }
//    }
//  }
//  def accepted(input: Long): Boolean = {
//    variables(0) = 0; variables(1) = 0; variables(2) = 0; variables(3) = 0; i = 0
//    val inp = input.toString()
//    for ((ins, targets) <- instructions) process(ins, targets, inp)
//    return variables(3) == 0
//  }
  
  var solutionMax = 0L
  var solutionMin = Long.MaxValue
  
  val firstAdd = (5 to (5+13*18) by 18).map( instructions(_)._2(1).toLong ).toArray
  val div = (4 to (4+13*18) by 18).map( instructions(_)._2(1).toLong ).toArray
  val secondAdd = (15 to (15+13*18) by 18).map( instructions(_)._2(1).toLong ).toArray
  
  val maxZ = div.indices.toArray.map( i => div.drop(i).foldLeft(1L)( _ * _ ) )
  
  def zAt(d: Int, w: Int, prevZ: Long): Long = {
    if (prevZ % 26L + firstAdd(d) == w.toLong) {
      return prevZ / div(d)
    } else {
      return 26L * (prevZ / div(d)) + w.toLong + secondAdd(d)
    }
  }
  
  def inner(d: Int, z: Long, input: Array[Char]): Unit = {
    if (d == 14) {
      if (z == 0L) {
        val sol = input.foldLeft("")( _ + _ ).toLong
        if (sol > solutionMax) {
          solutionMax = sol
        }
        if (sol < solutionMin) {
          solutionMin = sol
        }
      }
      return
    } else if (z >= maxZ(d)) {
      return
    }
    
    (1 to 9).foreach( i => {input(d) = ('0' + i).toChar; inner(d+1, zAt(d,i,z), input)} )
    return
  }
  
//  var input = 99999999999999L
//  while (!accepted(input) && input >= 11111111111111L) {
//    //println("Not accepted: " + input)
//    input -= 1
//    while (input.toString().contains('0')) input -= 1
//  }
  
  
  inner(0, 0L, new Array[Char](14))
  println("Largest accepted model number: " + solutionMax)
  println("Smallest accepted model number: " + solutionMin)
}