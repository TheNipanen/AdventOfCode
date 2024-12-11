package day11

import scala.io.Source
import scala.collection.mutable.HashMap

object PlutonianPebbles extends App {
  val s = Source.fromFile("src/day11/input.txt")
  val line = try s.getLines().toArray.apply(0) finally s.close()
  
  val stones = line.split(" ").toBuffer
  var stonesMap = HashMap[String, Long]()
  stones.foreach( stonesMap(_) = 1L )
  
  def blink() = {
    var i = 0
    while (i < stones.length) {
      val stone = stones(i)
      
      if (stone == "0") {
        stones(i) = "1"
      } else if (stone.length() % 2 == 0) {
        val (first, second) = stone.splitAt(stone.length() / 2)
        stones.insert(i, first.toLong.toString)
        i += 1
        stones(i) = second.toLong.toString
      } else {
        stones(i) = (stone.toLong * 2024L).toString
      }
      
      i += 1
    }
  }
  
  def addStones(map: HashMap[String, Long], stone: String, count: Long) = {
    if (map.contains(stone)) {
      map(stone) += count
    } else {
      map(stone) = count
    }
  }
  
  def blinkFast() = {
    val newMap = HashMap[String, Long]()
    for ((stone, count) <- stonesMap) {
      if (stone == "0") {
        addStones(newMap, "1", count)
      } else if (stone.length() % 2 == 0) {
        val (first, second) = stone.splitAt(stone.length() / 2)
        addStones(newMap, first.toLong.toString, count)
        addStones(newMap, second.toLong.toString, count)
      } else {
        addStones(newMap, (stone.toLong * 2024L).toString, count)
      }
    }
    stonesMap = newMap
  }
  
  var blinks = 0
  while (blinks < 25) {
    blink()
    blinks += 1
  }
  
  println(stones.length)
  
  blinks = 0
  while (blinks < 75) {
    blinkFast()
    blinks += 1
  }
  
  println(stonesMap.values.sum)
}