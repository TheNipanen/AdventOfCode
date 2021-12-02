

package day23

object CrabCups extends App {
//  var cups = Array(6,2,4,3,9,7,1,5,8)
//  var iterations = 0
//  var current = cups(0)
//  val min = cups.min
//  val m = cups.max
//  var l = m + 1
//  while (l <= 1000000) {
//    cups = cups ++ Array(l)
//    l += 1
//  }
//  val max = 1000000
//  def getThree: Array[Int] = {
//    var i = cups.indexOf(current)
//    var iter = 0
//    val res = scala.collection.mutable.ArrayBuffer[Int]()
//    while (iter < 3) {
//      i += 1
//      i = i % cups.length
//      res += cups(i)
//      iter += 1
//    }
//    res.toArray
//  }
  def getDest(three: Array[Long]): Long = {
    var d = current - 1
    if (d < min) d = max
    while (three.contains(d)) {
      d -= 1
      if (d < min) d = max
    }
    d
  }
//  while (iterations < 100) {
//    val three = getThree
//    cups = cups.filter( !three.contains(_) )
//    val dest = getDest
//    val i = cups.indexOf(dest)
//    val (start, end) = cups.splitAt(i + 1)
//    cups = start ++ three ++ end
//    val ii = cups.indexOf(current)
//    val iii = if (ii + 1 < cups.length) ii + 1 else 0
//    current = cups(iii)
//    iterations += 1
//  }
//  
//  val orig = cups.indexOf(1)
//  var i = if (orig + 1 < cups.length )orig + 1 else 0
//  val res = scala.collection.mutable.ArrayBuffer[Int]()
//  while (i != orig) {
//    res += cups(i)
//    i += 1
//    if (i == cups.length) i = 0
//  } 
//  println(res.foldLeft("")( _ + _ ))
  val orig = Array((6,2),(2,4),(4,3),(3,9),(9,7),(7,1),(1,5),(5,8),(8,6))
  val cups = scala.collection.mutable.HashMap[Long, Long]()
  orig.foreach( p => cups(p._1) = p._2 )
  var current = 6L
  var iterations = 0
  val min = 1L
  var max = 9L
  
  def getThree: Array[Long] = {
    val next1 = cups(current)
    val next2 = cups(next1)
    val next3 = cups(next2)
    val next4 = cups(next3)
    cups(current) = next4
    Array(next1,next2,next3)
  }
  def iteration = {
    val three = getThree
    val dest = getDest(three)
    val next = cups(dest)
    cups(dest) = three(0)
    cups(three(2)) = next
    current = cups(current)
    iterations += 1
  }
  while (iterations < 100) {
    iteration
  }
  var next = cups(1L)
  val res = scala.collection.mutable.ArrayBuffer[Long]()
  while (next != 1L) {
    res += next
    next = cups(next)
  }
  println("Values after 1 after 100 iterations: " + res.foldLeft("")( _ + _ ))
  
  iterations = 0
  max = 1000000L
  orig.foreach( p => cups(p._1) = p._2 )
  var n = 10L
  cups(8L) = 10L
  current = 6L
  while (n < 1000000L) {
    cups(n) = n + 1
    n += 1
  }
  cups(1000000L) = 6L
  while (iterations < 10000000) {
    iteration
  }
  val next1 = cups(1L)
  val next2 = cups(next1)
  val mult = next1 * next2
  println("Two values after 1 after 10000000 iterations: " + next1 + ", " + next2)
  println("Multiplied: " + mult)
}