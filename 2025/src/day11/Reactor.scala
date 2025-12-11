package day11
import scala.io.Source
import scala.collection.mutable.{ HashMap, HashSet }

object Reactor extends App {
  val s = Source.fromFile("src/day11/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val outputs = HashMap[String, Array[String]]()
  for (line <- lines) {
    val key = line.takeWhile( _ != ':' )
    val value = line.dropWhile( _ != ':' ).drop(2).split(" ")
    outputs(key) = value
  }
  
  def dfs(start: String, end: String) = {
    val path = HashSet[String]()
    val cache = HashMap[String, Long]()
    
    def visit(u: String): Long = {
      if (cache.contains(u)) {
        return cache(u)
      }
      if (u == end) {
        return 1
      }
      path(u) = true
      var sum = 0L
      for (neighbor <- outputs.getOrElse(u, Array())) {
        if (!path(neighbor)) {
          sum += visit(neighbor)
        }
      }
      path(u) = false
      cache(u) = sum
      sum
    }
    
    visit(start)
    cache(start)
  }
  
  val totalPaths = dfs("you", "out")
  println(totalPaths)
  
  val pathsSvrToDac = dfs("svr", "dac")
  val pathsSvrToFft = dfs("svr", "fft")
  val pathsDacToFft = dfs("dac", "fft")
  val pathsFftToDac = dfs("fft", "dac")
  val pathsDacToOut = dfs("dac", "out")
  val pathsFftToOut = dfs("fft", "out")
  
  // One of these will be zero
  val dacFirst = pathsSvrToDac * pathsDacToFft * pathsFftToOut
  val fftFirst = pathsSvrToFft * pathsFftToDac * pathsDacToOut
  println(dacFirst + fftFirst)
}