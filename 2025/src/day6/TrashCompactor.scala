package day6
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object TrashCompactor extends App {
  val s = Source.fromFile("src/day6/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val numberLines = lines.takeWhile( _.exists( _.isDigit ) ).map( _.split(' ').filter( _ != "" ) )
  val operatorLine = lines.takeRight(1)(0).split(' ').filter( _ != "" )
  val numbers = ArrayBuffer[String]()
  var i = 0
  val numberLinesWithSpace = lines.takeWhile( _.exists( _.isDigit ) )
  while (i < numberLinesWithSpace(0).size) {
    numbers += numberLinesWithSpace.map( _(i) ).mkString
    i += 1
  }
  val processedNumbers = ArrayBuffer[Array[Long]]()
  var currentBuffer = ArrayBuffer[Long]()
  for (current <- numbers.toArray.map( _.trim() )) {
    if (current == "") {
      processedNumbers += currentBuffer.toArray
      currentBuffer = ArrayBuffer[Long]()
    } else {
      currentBuffer += current.toLong
    }
  }
  processedNumbers += currentBuffer.toArray
  
  var sum = 0L
  var sum2 = 0L
  
  i = 0
  while (i < operatorLine.size) {
    val operator = operatorLine(i)
    val result =
      numberLines
      .map( _(i) )
      .foldLeft(if (operator == "*") 1L else 0L)({
        case (aggregate, current) => if (operator == "*") aggregate * current.toLong else aggregate + current.toLong
      })
    sum += result
    
    val result2 =
      processedNumbers(i)
      .foldLeft(if (operator == "*") 1L else 0L)({
        case (aggregate, current) => if (operator == "*") aggregate * current.toLong else aggregate + current.toLong
      })
    sum2 += result2
      
    i += 1
  }
  
  println(sum)
  println(sum2)
}