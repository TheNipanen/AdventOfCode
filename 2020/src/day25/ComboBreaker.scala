

package day25

object ComboBreaker extends App {
  val pk1 = 13233401L
  val pk2 = 6552760L
  val sNumber = 7L
  val divisor = 20201227L
  
  var loopSize1 = 0
  var res1 = 1L
  while (res1 != pk1) {
    res1 *= sNumber
    res1 = res1 % divisor
    loopSize1 += 1
  }
  
  var loopSize2 = 0
  var res2 = 1L
  while (res2 != pk2) {
    res2 *= sNumber
    res2 = res2 % divisor
    loopSize2 += 1
  }
  
  var ek1 = 1L
  var iterations = 0L
  while (iterations < loopSize1) {
    ek1 *= pk2
    ek1 = ek1 % divisor
    iterations += 1
  }
  
  var ek2 = 1L
  iterations = 0L
  while (iterations < loopSize2) {
    ek2 *= pk1
    ek2 = ek2 % divisor
    iterations += 1
  }
  
  assert(ek1 == ek2)
  println("Encryption key: " + ek1)
}