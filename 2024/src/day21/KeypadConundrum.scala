package day21

import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, HashMap, HashSet, Queue}

object KeypadConundrum extends App {
  val s = Source.fromFile("src/day21/input.txt")
  val codes = try s.getLines().toArray finally s.close()
  
  val numericCoords = HashMap(
    '7' -> (0, 0), '8' -> (1, 0), '9' -> (2, 0),
    '4' -> (0, 1), '5' -> (1, 1), '6' -> (2, 1),
    '1' -> (0, 2), '2' -> (1, 2), '3' -> (2, 2),
                   '0' -> (1, 3), 'A' -> (2, 3)
  )
  
  val directionalCoords = HashMap(
                   '^' -> (1, 0), 'A' -> (2, 0),
    '<' -> (0, 1), 'v' -> (1, 1), '>' -> (2, 1)
  )
  
  def manhattan(u: (Int, Int), v: (Int, Int)): Int = {
    math.abs(v._1 - u._1) + math.abs(v._2 - u._2)
  }
  
  def neighbors(u: (Int, Int), coords: HashSet[(Int, Int)]) = {
    Vector((u._1 + 1, u._2), (u._1 - 1, u._2), (u._1, u._2 + 1), (u._1, u._2 - 1)).filter( coords(_) )
  }
  
  val pathsCache = HashMap[(Char, Char), ArrayBuffer[Vector[(Int, Int)]]]()
  def pathsBetween(start: Char, end: Char): ArrayBuffer[Vector[(Int, Int)]] = {
    val key = (start, end)
    if (pathsCache.contains(key)) {
      return pathsCache(key)
    }
    
    val result = ArrayBuffer[Vector[(Int, Int)]]()
    val map = if (start.isDigit || end.isDigit) numericCoords else directionalCoords
    val set = HashSet(map.values.toSeq : _*)
    val dist = manhattan(map(start), map(end))
    val q = Queue[Vector[(Int, Int)]]()
    q.enqueue(Vector(map(start)))
    while (q.nonEmpty) {
      val u = q.dequeue()
      val last = u(u.length - 1)
      if (map(end) == last) {
        result += u
      } else if (u.length <= dist) {
        for (v <- neighbors(last, set)) {
          q.enqueue(u ++ Vector(v))
        }
      }
    }
    pathsCache(key) = result
    result
  }
  
  val coordsCache = HashMap[Vector[(Int, Int)], String]()
  def coordsToKeys(coords: Vector[(Int, Int)]): String = {
    if (coordsCache.contains(coords)) {
      return coordsCache(coords)
    }
    
    var result = ""
    var i = 1
    while (i < coords.length) {
      val (nextX, nextY) = coords(i)
      val (prevX, prevY) = coords(i - 1)
      val key = 
        if (nextX > prevX) '>'
        else if (nextX < prevX) '<'
        else if (nextY > prevY) 'v'
        else '^'
      result += key
      i += 1
    }
    result + 'A'
  }
  
  val pressesCache = HashMap[(String, Int), Long]()
  def leastPresses(code: String, depth: Int): Long = {
    val key = (code, depth)
    if (pressesCache.contains(key)) {
      return pressesCache(key)
    }
    
    var result = 0L
    var current = 'A'
    for (c <- code) {
      val paths = pathsBetween(current, c)
      if (depth == 0) {
        result += paths.map( _.length ).min.toLong
      } else {
        var best = Long.MaxValue
        for (path <- paths) {
          val keys = coordsToKeys(path)
          best = best min leastPresses(keys, depth - 1)
        }
        result += best
      }
      current = c
    }
    pressesCache(key) = result
    result
  }
  
  var sum1 = 0L
  var sum2 = 0L
  for (code <- codes) {
    val presses1 = leastPresses(code, 2)
    val presses2 = leastPresses(code, 25)
    val number = code.dropRight(1).toLong
    val complexity1 = number * presses1
    val complexity2 = number * presses2
    sum1 += complexity1
    sum2 += complexity2
  }
  
  println(sum1)
  println(sum2)
}