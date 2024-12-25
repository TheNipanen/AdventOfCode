package day24

import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet}

object CrossedWires extends App {
  val s = Source.fromFile("src/day24/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val initialValues = lines.takeWhile( _ != "" )
  val gates = lines.dropWhile( _ != "" ).drop(1)
  
  val values = HashMap[String, String]()
  val wiresStartingZ = HashSet[String]()
  val gateWires = ArrayBuffer[String]()
  val computedValues = HashMap[String, Boolean]()
  
  for (line <- initialValues) {
    val wire = line.takeWhile( _ != ':' )
    val value = line.dropWhile( _ != ' ' ).drop(1)
    computedValues(wire) = value.toInt == 1
    if (wire.startsWith("z")) wiresStartingZ(wire) = true
  }
  for (line <- gates) {
    val gate = line.takeWhile( _ != '-' ).trim
    val wire = line.takeRight(3)
    values(wire) = gate
    if (wire.startsWith("z")) wiresStartingZ(wire) = true
    gateWires += wire
  }
  
  implicit def bool2string(b:Boolean) = if (b) "1" else "0"
    
  def longFromBits(values: HashMap[String, String], computedValues: HashMap[String, Boolean], bits: Vector[String]): Long = {
    val binary = bits.sorted.reverse.map( computeValue(values, computedValues, _):String ).mkString("")
    BigInt(binary, 2).longValue()
  }
  def computeValue(values: HashMap[String, String], computedValues: HashMap[String, Boolean], wire: String): Boolean = {
    if (computedValues.contains(wire)) {
      return computedValues(wire)
    }

    val value = values(wire).split(" ")
    val first = computeValue(values, computedValues, value(0))
    val second = computeValue(values, computedValues, value(2))
    val op = value(1)
    val result =
      if (op == "AND") first && second
      else if (op == "OR") first || second
      else if (op == "XOR") (first && !second) || (!first && second)
      else throw new Error("bad")
    computedValues(wire) = result
    result
  }
  
  def gatesForOutput(wire: String): Vector[String] = {
    if (!values.contains(wire)) return Vector()
    
    val value = values(wire).split(" ")
    Vector(wire) ++ gatesForOutput(value(0)) ++ gatesForOutput(value(2))
  }
  
  val res = longFromBits(values.clone(), computedValues.clone(), wiresStartingZ.toVector)
  println(res)
  
  val wiresStartingX = initialValues.take(45).map( _.takeWhile( _ != ':' ) )
  val wiresStartingY = initialValues.drop(45).map( _.takeWhile( _ != ':' ) )
  val x = longFromBits(values.clone(), computedValues.clone(), wiresStartingX.toVector)
  val y = longFromBits(values.clone(), computedValues.clone(), wiresStartingY.toVector)
  val correct = x + y
  
  def swap (s1: String, s2: String) = {
    val tmp = values(s1)
    values(s1) = values(s2)
    values(s2) = tmp
  }
  
  val invalidEndWires = wiresStartingZ.filter( wire => wire != "z45" && values(wire).split(" ")(1) != "XOR" ).toVector
  val invalidMidWires = gateWires.filter( wire => {
    val value = values(wire).split(" ")
    !wire.startsWith("z") && value(1) == "XOR" &&
    !value(0).startsWith("x") && !value(0).startsWith("y") &&
    !value(2).startsWith("x") && !value(2).startsWith("y")
  })
  
  val swaps = invalidEndWires.map( wire => (wire, gatesForOutput("z" + (wire.takeRight(2).toInt + 1)).filter( invalidMidWires.contains(_) )(0)) )
  for ((s1, s2) <- swaps) {
    swap(s1, s2)
  }
  
  var diff = correct ^ longFromBits(values.clone(), computedValues.clone(), wiresStartingZ.toVector)
  val r = new util.Random()
  while (diff == 0L) {
    val newX = BigInt(math.abs(r.nextLong()).toBinaryString.takeRight(45), 2).longValue()
    val newY = BigInt(math.abs(r.nextLong()).toBinaryString.takeRight(45), 2).longValue()
    val newCorrect = newX + newY
    for (i <- 0 to 44) {
      val xWire = "x" + ("0" + i).takeRight(2)
      val yWire = "y" + ("0" + i).takeRight(2)
      computedValues(xWire) = ((newX >>> i) & 1L) == 1L
      computedValues(yWire) = ((newY >>> i) & 1L) == 1L
    }
    diff = newCorrect ^ longFromBits(values.clone(), computedValues.clone(), wiresStartingZ.toVector)
  }
  
  val zeroes = ("0" + diff.toBinaryString.reverse.takeWhile( _ == '0' ).length()).takeRight(2)
  val invalidCarryWires = values.keys.filter( wire => {val value = values(wire).split(" "); value(0).endsWith(zeroes) && value(2).endsWith(zeroes) }).toVector
  
  println((invalidEndWires ++ invalidMidWires ++ invalidCarryWires).sorted.mkString(","))
}