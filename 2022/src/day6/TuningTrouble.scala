package day6

import scala.io.Source
import scala.collection.mutable.Queue

object TuningTrouble extends App {
  val s = Source.fromFile("src/day6/input.txt")
  val buffer = try s.getLines().toArray.apply(0) finally s.close()
  
  val q = Queue[Char]()
  val q2 = Queue[Char]()
  var i = 0
  val l = buffer.length()
  var packetFound = false
  var messageFound = false
  while (i < l && (!packetFound || !messageFound)) {
    val c = buffer(i)
    i += 1
    if (q.length == 4) {
      q.dequeue()
    }
    q.enqueue(c)
    if (!packetFound && q.length == 4 && q.distinct.length == q.length) {
      println("Start of packet at: " + i)
      packetFound = true
    }
    if (q2.length == 14) {
      q2.dequeue()
    }
    q2.enqueue(c)
    if (!messageFound && q2.length == 14 && q2.distinct.length == q2.length) {
      println("Start of message at: " + i)
      messageFound = true
    }
  }
}