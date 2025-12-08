package day8
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import lib.UnionFind

object Playground extends App {
  val s = Source.fromFile("src/day8/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val pairs = ArrayBuffer[(String, String)]()
  
  val l = lines.size
  var i = 0
  while (i < l) {
    var j = i + 1
    while (j < l) {
      pairs += ((lines(i), lines(j)))
      j += 1
    }
    i += 1
  }
  
  def dist(pair: (String, String)) = {
    val first = pair._1.split(",")
    val second = pair._2.split(",")
    
    val xDiff = first(0).toLong - second(0).toLong
    val yDiff = first(1).toLong - second(1).toLong
    val zDiff = first(2).toLong - second(2).toLong
    
    math.sqrt( xDiff * xDiff + yDiff * yDiff + zDiff * zDiff )
  }
  
  val sortedPairs = pairs.sortBy(dist)
  
  val uf = new UnionFind[String]()
  
  for (box <- lines) {
    uf.makeSet(box)
  }
  
  i = 0
  while (i < 1000) {
    val (first, second) = sortedPairs(i)
    uf.union(first, second)
    i += 1
  }
  
  val circuits = uf.getSets().sortBy( -_.size )
  println(circuits(0).size.toLong * circuits(1).size.toLong * circuits(2).size.toLong)
  
  while (uf.nofSets > 1) {
    val (first, second) = sortedPairs(i)
    uf.union(first, second)
    i += 1
  }
  
  val lastPair = sortedPairs(i - 1)
  println(lastPair._1.split(",")(0).toLong * lastPair._2.split(",")(0).toLong)
}