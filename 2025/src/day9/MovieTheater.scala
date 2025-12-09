package day9
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object MovieTheater extends App {
  val s = Source.fromFile("src/day9/input.txt")
  val lines = try s.getLines().toArray.map( line => {val tile = line.split(","); (tile(0).toLong, tile(1).toLong)} ) finally s.close()
  
  var maxRectangleArea = 0L
  var maxRedGreenArea = 0L
  
  val l = lines.size
  
  val edges = ArrayBuffer[((Long, Long), (Long, Long))]((lines(l - 1), lines(0)))
  
  var i = 1
  while (i < l) {
    edges += ((lines(i), lines(i - 1)))
    i += 1
  }
  
  def intervalsIntersect(first: (Long, Long), second: (Long, Long)): Boolean = {
    val firstStart = first._1 min first._2
    val firstEnd = first._1 max first._2
    val secondStart = second._1 min second._2
    val secondEnd = second._1 max second._2
    return firstStart < secondEnd && secondStart < firstEnd
  }
  
  def intersectsRect(edgeFirst: (Long, Long), edgeSecond: (Long, Long), rectFirst: (Long, Long), rectSecond: (Long, Long)): Boolean = {
    if (!intervalsIntersect((edgeFirst._1, edgeSecond._1), (rectFirst._1, rectSecond._1))) {
      return false
    }
    if (!intervalsIntersect((edgeFirst._2, edgeSecond._2), (rectFirst._2, rectSecond._2))) {
      return false
    }
    
    true
  }
  
  i = 0
  while (i < l) {
    val (x1, y1) = lines(i)
    
    var j = i + 1
    while (j < l) {
      val (x2, y2) = lines(j)
      
      val area = (math.abs(x1 - x2) + 1L) * (math.abs(y1 - y2) + 1L)
      maxRectangleArea = maxRectangleArea max area
      
      if (area > maxRedGreenArea) {
        val rectEdges = Array(((x1, y1), (x1, y2)), ((x1, y1), (x2, y1)), ((x2, y2), (x2, y1)), ((x2, y2), (x1, y2)))
        if (!edges.exists( edge => intersectsRect(edge._1, edge._2, (x1, y1), (x2, y2)) )) {
          maxRedGreenArea = area
        }
      }
      
      j += 1
    }
    i += 1
  }
  
  println(maxRectangleArea)
  println(maxRedGreenArea)
}