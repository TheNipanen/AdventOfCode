package day2

import scala.io.Source

object RedNosedReports extends App {
  val s = Source.fromFile("src/day2/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val reports = lines.map( _.split("\\s+").map( _.toInt ) )
  val l = reports.length
  
  def isSafe(report: Array[Int]): Boolean = {
    val reportLength = report.length
    var j = 1
    var allIncreasing = true
    var allDecreasing = true
    while (j < reportLength) {
      val level = report(j)
      val prevLevel = report(j - 1)
      if (level - prevLevel < 1 || level - prevLevel > 3) {
        allIncreasing = false
      }
      if (level - prevLevel > -1 || level - prevLevel < - 3) {
        allDecreasing = false
      }
      j += 1
    }
    allIncreasing || allDecreasing
  }
  
  var safeReports = 0
  var safeDampenedReports = 0
  var i = 0
  while (i < l) {
    val report = reports(i)
    
    if (isSafe(report)) {
      safeReports += 1
      safeDampenedReports += 1
    } else {
      var found = false
      var j = 0
      val reportLength = report.length
      while (!found && j < reportLength) {
        val dampenedReport = report.take(j) ++ report.drop(j + 1)
        if (isSafe(dampenedReport)) {
          found = true
          safeDampenedReports += 1
        }
        j += 1
      }
    }
    
    i += 1
  }
  
  println(safeReports)
  println(safeDampenedReports)
}