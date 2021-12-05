package day6

import scala.io.Source

object CustomCustoms extends App {
  val s = Source.fromFile("src/day6/input.txt")
  val reader = s.bufferedReader()
  
  var totalCount = 0
  var line = reader.readLine()
  while (line != null) {
    var people = 0
    val yes = scala.collection.mutable.HashMap[Char, Int]()
    while (line != "" && line != null) {
      line.foreach( c => {
        if (yes.contains(c)) yes(c) += 1
        else yes(c) = 1
      })
      people += 1
      line = reader.readLine()
    }
    val count = yes.filter( _._2 == people ).size
    totalCount += count
    line = reader.readLine()
  }
  s.close()
  println(totalCount)
}