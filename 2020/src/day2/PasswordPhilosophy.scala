package day2

import scala.io.Source

object PasswordPhilosophy extends App {
  // How many passwords satisfy their policy
  var valid = 0
  Source.fromFile("src/day2/input.txt").getLines().foreach(s => {
    val i = s.indexOf('-')
    val (first, rest) = s.splitAt(i)
    val low = first.toInt
    
    val ii = rest.indexOf(' ')
    val (second, pass) = rest.splitAt(ii)
    val high = second.drop(1).toInt
    
    val c = pass.drop(1)(0)
    val password = pass.reverse.takeWhile( _ != ' ' ).reverse
    
    val count = password.count( _ == c )
    if (count >= low && count <= high) valid += 1
  })
  println("Valid passwords: " + valid)
  
  // New policy
  valid = 0
  Source.fromFile("src/day2/input.txt").getLines().foreach(s => {
    val i = s.indexOf('-')
    val (first, rest) = s.splitAt(i)
    val low = first.toInt
    
    val ii = rest.indexOf(' ')
    val (second, pass) = rest.splitAt(ii)
    val high = second.drop(1).toInt
    
    val c = pass.drop(1)(0)
    val password = pass.reverse.takeWhile( _ != ' ' ).reverse
    
    var count = 0
    if (password(low - 1) == c) count += 1
    if (password(high - 1) == c) count += 1
    if (count == 1) valid += 1
  })
  println("Valid passwords: " + valid)
}