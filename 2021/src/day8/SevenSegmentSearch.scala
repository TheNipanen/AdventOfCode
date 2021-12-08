package day8

import scala.io.Source
import scala.collection.mutable.HashMap

object SevenSegmentSearch extends App {
  val s = Source.fromFile("src/day8/input.txt")
  val lines = s.getLines().toArray
  s.close()
  
  val digits = lines.map( _.split('|')(1).trim.split(" ") ).flatten
  val uniqueDigits = digits.count( d => d.size == 2 || d.size == 4 || d.size == 3 || d.size == 7 )
  println("Unique digits: " + uniqueDigits)
  
  def decode(line: String): Int = {
    val s = line.split('|')
    val patterns = s(0).trim.split(" ").map( _.sorted )
    val patternToDigit = HashMap[String, Int]()
    
    val one = patterns.find( _.size == 2 ).get
    patternToDigit(one) = 1
    
    val four = patterns.find( _.size == 4 ).get
    patternToDigit(four) = 4
    
    val seven = patterns.find( _.size == 3 ).get
    patternToDigit(seven) = 7
    
    val eight = patterns.find( _.size == 7 ).get
    patternToDigit(eight) = 8
    
    val nine = patterns.find( p => p.size == 6 && four.forall( p.contains(_) ) ).get
    patternToDigit(nine) = 9
    
    val rightChars = seven.filter( one.contains(_) )
    val zero = patterns.find( p => p.size == 6 && p != nine && rightChars.forall(p.contains(_)) ).get
    patternToDigit(zero) = 0
    
    val six = patterns.find( p => p.size == 6 && p != nine && p != zero ).get
    patternToDigit(six) = 6
    
    val three = patterns.find( p => p.size == 5 && rightChars.forall(p.contains(_)) ).get
    patternToDigit(three) = 3
    
    val two = patterns.find( p => p.size == 5 && p != three && four.count(p.contains(_)) == 2 ).get
    patternToDigit(two) = 2
    
    val five = patterns.find( p => p.size == 5 && p != three && p != two ).get
    patternToDigit(five) = 5
    
    val outDigits = s(1).trim.split(" ").map( str => patternToDigit(str.sorted) )
    val out = outDigits(0) * 1000 + outDigits(1) * 100 + outDigits(2) * 10 + outDigits(3)
    out
  }
  
  val sum = lines.map( decode(_) ).sum
  println("Output sum: " + sum)
}