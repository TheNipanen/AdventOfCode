package day2
import scala.io.Source

object GiftShop extends App {
  val s = Source.fromFile("src/day2/input.txt")
  val line = try s.getLines().next() finally s.close()
  
  var sum = 0L
  var sum2 = 0L
  
  val ranges = line.split(",")
  for (range <- ranges) {
    val i = range.indexOf("-")
    val start = range.take(i).toLong
    val end = range.drop(i + 1).toLong
    for (id <- start to end) {
      val asString = id.toString()
      val length = asString.size
      val half = length / 2
      if (length % 2 == 0 && asString.take(half) == asString.drop(half)) {
        sum += id
      }
      
      var found = false
      for (checkingLength <- 1 to half) {
        if (!found && length % checkingLength == 0) {
          val parts = asString.grouped(checkingLength).toArray
          if (parts.zipWithIndex.forall({case (part, i) => i == 0 || part == parts(i - 1)})) {
            found = true
            sum2 += id
          }
        }
      }
    }
  }
  
  println(sum)
  println(sum2)
}