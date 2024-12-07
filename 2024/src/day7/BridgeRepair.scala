package day7

import scala.io.Source

object BridgeRepair extends App {
  val s = Source.fromFile("src/day7/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val equations = lines.map((line) => {
    val testValue = line.takeWhile( _.isDigit ).toLong
    val numbers = line.dropWhile( _.isDigit ).dropWhile( !_.isDigit ).split(" ").map( _.toLong )
    (testValue, numbers)
  })
  
  def possibleCombinations(testValue: Long, numbers: Array[Long], concat: Boolean): Long = {
    if (numbers.length == 1) {
      return numbers.count( _ == testValue )
    }
    
    val first = numbers(0) + numbers(1)
    val second = numbers(0) * numbers(1)
    val third = (numbers(0).toString() + numbers(1).toString()).toLong
    val rest = numbers.drop(2)
    val concatCombinations = if (concat) possibleCombinations(testValue, Array(third) ++ rest, concat) else 0
    return possibleCombinations(testValue, Array(first) ++ rest, concat) + possibleCombinations(testValue, Array(second) ++ rest, concat) + concatCombinations
  }
  
  var calibrationResult = 0L
  var concatCalibrationResult = 0L
  
  val l = equations.length
  var i = 0
  while (i < l) {
    val (testValue, numbers) = equations(i)
    val combinations = possibleCombinations(testValue, numbers, false)
    if (combinations > 0L) {
      calibrationResult += testValue
    }
    val concatCombinations = possibleCombinations(testValue, numbers, true)
    if (concatCombinations > 0L) {
      concatCalibrationResult += testValue
    }
    i += 1
  }
  
  println(calibrationResult)
  println(concatCalibrationResult)
}