package day5
import scala.io.Source
import scala.collection.mutable.HashSet

object Cafeteria extends App {
  val s = Source.fromFile("src/day5/input.txt")
  val lines = try s.getLines().toArray finally s.close()

  val ranges = lines.takeWhile(_ != "")

  var nOfFreshIds = 0L

  val availableIds = lines.dropWhile(_ != "").drop(1)
  for (id <- availableIds) {
    var fresh = false
    var i = 0
    while (!fresh && i < ranges.size) {
      val start = ranges(i).takeWhile(_ != '-').toLong
      val end = ranges(i).dropWhile(_ != '-').drop(1).toLong
      if (id.toLong >= start && id.toLong <= end) {
        fresh = true
        nOfFreshIds += 1
      }
      i += 1
    }
  }
  println(nOfFreshIds)
  
  var mergedRanges = ranges.sortBy( _.takeWhile( _ != '-' ).toLong ).map( range => (range.takeWhile( _ != '-' ).toLong, range.dropWhile( _ != '-' ).drop(1).toLong) )
  def mergeRound() = {
    var mergeHappened = false
    var i = 0
    while (i < mergedRanges.size - 1) {
      val (startFirst, endFirst) = mergedRanges(i)
      val (startSecond, endSecond) = mergedRanges(i + 1)
      if (startSecond <= endFirst) {
        mergeHappened = true
        mergedRanges(i) = (startFirst, endFirst max endSecond)
        mergedRanges(i + 1) = (Long.MinValue, Long.MinValue)
        i += 1
      }
      i += 1
    }
    if (mergeHappened) {
      mergedRanges = mergedRanges.filter( _._1 != Long.MinValue )
    }
    mergeHappened
  }
  
  while (mergeRound()) {}
  
  val possibleFresh = mergedRanges.foldLeft(0L)({ case (sum, (start, end)) => sum + (end - start + 1) })
  println(possibleFresh)
}