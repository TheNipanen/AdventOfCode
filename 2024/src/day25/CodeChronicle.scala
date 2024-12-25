package day25

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object CodeChronicle extends App {
  val s = Source.fromFile("src/day25/input.txt")
  val lines = try s.getLines().toArray ++ Array("") finally s.close()
  val blobs = lines.foldLeft((Vector[Vector[String]](), Vector[String]()))((arrs, next) => if (next == "") (arrs._1 ++ Vector(arrs._2), Vector()) else (arrs._1, arrs._2 ++ Vector(next)))._1
  
  val locks = blobs.filter( _(0) == "#####" )
  val keys = blobs.filter( _(0) == "....." )
  
  def fits(lock: Vector[String], key: Vector[String]): Boolean = {
    for (i <- 0 to 4) {
      val lockLen = lock.count( _(i) == '#' )
      val keyLen = key.count( _(i) == '#' )
      if (lockLen + keyLen > 7) return false
    }
    true
  }
  
  var fitPairs = 0
  var i = 0
  while (i < locks.length) {
    var j = 0
    while (j < keys.length) {
      if (fits(locks(i), keys(j))) fitPairs += 1
      j += 1
    }
    i += 1
  }
  println(fitPairs)
}