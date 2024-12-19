package day19

import scala.io.Source
import scala.collection.mutable.HashMap

object LinenLayout extends App {
  val s = Source.fromFile("src/day19/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val patterns = lines(0).split(", ")
  val designs = lines.drop(2)
  
  val waysFor = HashMap[String, Long]()
  
  def tryDesign(design: String): Long = {
    var remaining = design
    
    def step(): Long = {
      if (remaining == "") return 1
      if (waysFor.contains(remaining)) return waysFor(remaining)
      val options = patterns.filter( remaining.startsWith(_) )
      var count = 0L
      for (op <- options) {
        remaining = remaining.replaceFirst(op, "")
        count += step()
        remaining = op + remaining
      }
      waysFor(remaining) = count
      count
    }
    
    step()
  }
  
  var possible = 0
  var ways = 0L
  var i = 0
  while (i < designs.length) {
    val count = tryDesign(designs(i))
    if (count > 0) possible += 1
    ways += count
    i += 1
  }
  
  println(possible)
  println(ways)
}