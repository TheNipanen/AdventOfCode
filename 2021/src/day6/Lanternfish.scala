package day6

import scala.io.Source

object Lanternfish extends App {
  val s = Source.fromFile("src/day6/input.txt")
  var fishOfTimer = new Array[Int](9)
  s.getLines().toArray.apply(0).split(",").map( _.toInt ).foreach( fishOfTimer(_) += 1 )
  s.close()
  var fishOfTimer2 = fishOfTimer.clone().map( _.toLong )
  
  var i = 0
  while (i < 80) {
    val nextFishOfTimer = new Array[Int](9)
    var j = 0
    while (j < 8) {
      nextFishOfTimer(j) = fishOfTimer(j+1)
      j += 1
    }
    nextFishOfTimer(6) += fishOfTimer(0)
    nextFishOfTimer(8) = fishOfTimer(0)
    fishOfTimer = nextFishOfTimer
    i += 1
  }
  
  val nOfFish = fishOfTimer.sum
  println("Lanternfish after 80 days: " + nOfFish)
  
  i = 0
  while (i < 256) {
    val nextFishOfTimer = new Array[Long](9)
    var j = 0
    while (j < 8) {
      nextFishOfTimer(j) = fishOfTimer2(j+1)
      j += 1
    }
    nextFishOfTimer(6) += fishOfTimer2(0)
    nextFishOfTimer(8) = fishOfTimer2(0)
    fishOfTimer2 = nextFishOfTimer
    i += 1
  }
  
  val nOfFish2 = fishOfTimer2.sum
  println("Lanternfish after 256 days: " + nOfFish2)
}