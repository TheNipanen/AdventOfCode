package day12

import scala.io.Source
import scala.collection.mutable.HashMap

object HotSprings extends App {
  val s = Source.fromFile("src/day12/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val maxUnknowns = lines.map( _.count( _ == '?' ) ).max + 1
  val combies = HashMap[Int, Array[String]]()
  var combs = Array(".", "#")
  for (unknowns <- 1 to maxUnknowns) {
    combies(unknowns) = combs
    combs = combs.map( "." + _ ) ++ combs.map( "#" + _ )
  }
  
  var sum = 0L
  var sum2 = 0L
  
  def fits(str: String, criteria: Array[Int]): Boolean = {
    val brokenParts = str.split("\\.").filter( _.contains('#') ).map( _.length )
    brokenParts.sameElements(criteria)
  }
  
  def doCombs(combinations: Array[String], springs: String, criteria: Array[Int]): Long = {
    var possibilities = 0L
    for (comb <- combinations) {
      var j = 0
      val substituted = springs.foldLeft("")( (str, c) => { 
        try {
          var res = str + c
          if (c == '?') {
            res = str + comb(j)
            j += 1
          }
          res
        } catch {
          case _: ArrayIndexOutOfBoundsException => str + c
        }
      })
      if (fits(substituted, criteria)) {
        possibilities += 1L
      }
    }
    possibilities
  }
  
  val l = lines.length
  var i = 0
  while (i < l) {
    val line = lines(i)
    val split = line.split(" ")
    val springs = split(0)
    val criteria = split(1).split(",").map( _.toInt )
    val unknowns = springs.count( _ == '?' )
    val combinations = combies(unknowns)
    val possibilities = doCombs(combinations, springs, criteria)
    sum += possibilities
    
    val springs2 = springs + "?"
    val possibilities2 = doCombs(combies(unknowns + 1), springs2, criteria)
    sum2 += possibilities2 * possibilities2 * possibilities2 * possibilities2 * possibilities
    i += 1
  }
  
  println("Sum of row possibilities: " + sum)
  println("Sum of big row possibilities: " + sum2)
}