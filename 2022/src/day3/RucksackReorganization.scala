package day3

import scala.io.Source

object RucksackReorganization extends App {
  val s = Source.fromFile("src/day3/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val p = scala.collection.mutable.Map[Char, Int]()
  var i = 1
  for (c <- 'a' to 'z') {
    p(c) = i
    i += 1
  }
  for (c <- 'A' to 'Z') {
    p(c) = i
    i += 1
  }
  
  var sum = 0
  i = 0
  while (i < lines.length) {
    val line = lines(i)
    val l = line.length() / 2
    val first = line.take(l)
    val second = line.drop(l)
    val item = first.find( second.contains(_) ).get
    sum += p(item)
    i += 1
  }
  
  println(sum)
  
  sum = 0
  i = 0
  while (i < lines.length) {
    val line1 = lines(i)
    val line2 = lines(i+1)
    val line3 = lines(i+2)
    val item = line1.find( c => line2.contains(c) && line3.contains(c) ).get
    sum += p(item)
    i += 3
  }
  
  println(sum)
}