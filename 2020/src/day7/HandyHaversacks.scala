package day7

import scala.io.Source

object HandyHaversacks extends App {
  val s = Source.fromFile("src/day7/input.txt")
  val reader = s.bufferedReader()
  
  val bagContains = scala.collection.mutable.HashMap[String, Array[(Int, String)]]()
  var line = reader.readLine()
  while (line != null) {
    val parts = line.split("contain")
    val start = parts(0).dropRight(2)
    val innards = parts(1).split(",").map( p => {
      val empty = p == " no other bags."
      val amount = if (empty) 0 else p.drop(1).takeWhile( _.isDigit ).toInt
      val bag = if (empty) "" else p.drop(3).reverse.dropWhile( _ != 'g' ).reverse
      (amount, bag)
    })
    bagContains(start) = innards
    line = reader.readLine()
  }
  s.close()
  
  def inner(bag: String): Int = {
    val innards = bagContains(bag)
    var c = 0
    for ((amount, other) <- innards) {
      if (amount == 0) return 0
      if (other == "shiny gold bag") c += amount
      val next = inner(other)
      c += amount * next
    }
    return c
  }
  
  val bags = bagContains.keys
  var count = 0
  for (bag <- bags) {
    val c = inner(bag)
    if (c > 0)count += 1
  }
  println("Bags that can contain shiny gold bags: " + count)
  
  def inner2(bag: String): Long = {
    val innards = bagContains(bag)
    var c = 0L
    for ((amount, other) <- innards) {
      //println(amount + "  " + other)
      if (amount == 0) return 0
      val next = inner2(other)
      c += amount + amount * next
    }
    return c
  }
  val count2 = inner2("shiny gold bag")
  //val ss = bagContains("shiny gold bag")
  //ss.foreach( println(_) )
  println("Shiny gold bag contains " + count2 + " bags")
}