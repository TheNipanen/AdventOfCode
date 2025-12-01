package day1
import scala.io.Source

object SecretEntrance extends App {
	val s = Source.fromFile("src/day1/input.txt")
	val lines = s.getLines().toArray
	
	var current = 50
	var zeroes = 0
	var crossings = 0
	for (line <- lines) {
	  val dir = line.head
	  var count = line.tail.toInt
	  if (dir == 'L') count = -count
	  val prev = current
	  current += count
	  crossings += math.abs(current / 100)
	  if (current > 0 && current % 100 == 0) {
	    crossings -= 1
	  }
	  current %= 100
	  if (current < 0) {
	    if (prev != 0) {
	      crossings += 1
	    }
	    current += 100
	  }
	  if (current == 0) {
	    zeroes += 1
	  }
	}
	println(zeroes)
	println(zeroes + crossings)
}