package day25

import scala.io.Source

object FullOfHotAir extends App {
  val s = Source.fromFile("src/day25/input.txt")
  val SNAFUs = try s.getLines().toArray finally s.close()
  
  val value = Map('=' -> -2L, '-' -> -1L, '0' -> 0L, '1' -> 1L, '2' -> 2L)
  def parse(n: String) = {
    var k = 1L
    var j = n.length - 1
    var res = 0L
    while (j >= 0) {
      res += value(n(j)) * k
      k *= 5
      j -= 1
    }
    res
  }
  
  def nToSNAFU(n: Long): String = {
    if (n == 0L) {
      return ""
    }
    
    val mod = n % 5L
    mod match {
      case 0 | 1 | 2 => nToSNAFU(n / 5) + mod.toString()
      case 3 => nToSNAFU(1 + n / 5) + "="
      case 4 => nToSNAFU(1 + n / 5) + "-"
    }
  }
  
  var sum = 0L
  var i = 0
  while (i < SNAFUs.length) {
    val n = parse(SNAFUs(i))
    sum += n
    i += 1
  }
  
  val res = nToSNAFU(sum)
  println("SNAFU number to supply: " + res)
}