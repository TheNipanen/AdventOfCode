

package day15

object RambunctiousRecitation extends App {
  val numbers = scala.collection.mutable.ArrayBuffer(0,6,1,7,2,19,20)
  var prev = 20
  var i = 7
  while (i < 2020) {
    var j = numbers.length - 2
    var found = false
    while (j >= 0 && !found) {
      if (numbers(j) == prev) found = true
      else j -= 1
    }
    var current = -1
    if (j == -1) {
      current = 0
    } else {
      current = i - j - 1
    }
    numbers += current
    prev = current
    i += 1
  }
  println("2020th number: " + numbers(2019))
  
  val previous = scala.collection.mutable.HashMap[Int, Int]((0,0),(6,1),(1,2),(7,3),(2,4),(19,5),(20,6))
  prev = 20
  i = 7
  while (i < 30000000) {
    val p = previous(prev)
    var current = -1
    if (p == -1) {
      current = 0
    } else {
      current = i - p - 1
    }
    previous(prev) = i - 1
    if (!previous.contains(current)) previous(current) = -1
    prev = current
    i += 1
  }
  
  println("30000000th number: " + prev)
}