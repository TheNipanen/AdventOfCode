package day11

import scala.io.Source
import scala.collection.mutable.Queue
import scala.math.BigInt

class ItemWithModulos(initialValue: Int, val divisors: Array[Int]) {
  val values = divisors.map( initialValue % _ )
  
  def updateValues(op: Int => Int) = {
    for (i <- 0 until divisors.length) {
      values(i) = op(values(i)) % divisors(i)
    }
  }
  
  def isDivisible(i: Int) = {
    values(i) == 0
  }
}

object MonkeyInTheMiddle extends App {
  val s = Source.fromFile("src/day11/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  val monkeys = lines.filter( _ != "" ).grouped(6).toArray
  
  def parseOperation(op: String): Int => Int = {
    val parts = op.split(" ")
    val second = if (parts(2) == "old") None else Some(parts(2).toInt)
    val operation = 
      if (parts(1) == "+") {
        (old: Int) => old + (if (second.isEmpty) old else second.get)
      } else if (parts(1) == "*") {
        (old: Int) => old * (if (second.isEmpty) old else second.get)
      } else {
        throw new UnsupportedOperationException("Operation not implemented")
      }
    operation
  }
  
  val items = Array.fill(monkeys.length)(Queue[Int]())
  val operation: Array[Int => Int] = Array.fill(monkeys.length)( (old: Int) => old)
  val divisor = Array.fill(monkeys.length)(0)
  val action = Array.fill(monkeys.length)((0,0))
  val inspections = Array.fill(monkeys.length)(0L)
  
  for (i <- 0 until monkeys.length) {
    val monkey = monkeys(i)
    monkey(1).drop(18).split(", ").map( _.toInt ).foreach( items(i).enqueue(_) )
    operation(i) = parseOperation(monkey(2).drop(19))
    divisor(i) = monkey(3).drop(21).toInt
    action(i) = (monkey(4).drop(29).toInt, monkey(5).drop(30).toInt)
  }
  
  val items2 = items.map( _.clone().map( new ItemWithModulos(_, divisor) ) )
  val inspections2 = inspections.clone()
  
  def playTurn(i: Int) = {
    while (items(i).nonEmpty) {
      var item = items(i).dequeue()
      item = operation(i)(item)
      item /= 3
      val throwTo = if (item % divisor(i) == 0) action(i)._1 else action(i)._2
      items(throwTo).enqueue(item)
      inspections(i) += 1
    }
  }
  def playTurn2(i: Int) = {
    while (items2(i).nonEmpty) {
      var item2 = items2(i).dequeue()
      item2.updateValues(operation(i))
      val throwTo2 = if (item2.isDivisible(i)) action(i)._1 else action(i)._2
      items2(throwTo2).enqueue(item2)
      inspections2(i) += 1
    }
  }
  
  var rounds = 0
  while (rounds < 10000) {
    if (rounds < 20) {
      for (i <- 0 until monkeys.length) {
        playTurn(i)
      }
    }
    for (i <- 0 until monkeys.length) {
      playTurn2(i)
    }
    rounds += 1
  }
  
  val sortedInspections = inspections.sortBy( -_ )
  val first = sortedInspections(0)
  val second = sortedInspections(1)
  val monkeyBusiness = first * second
  println("Level of monkey business after 20 rounds: " + monkeyBusiness)
  
  val sortedInspections2 = inspections2.sortBy( -_ )
  val first2 = sortedInspections2(0)
  val second2 = sortedInspections2(1)
  val monkeyBusiness2 = first2 * second2
  println("Level of monkey business after 10000 rounds: " + monkeyBusiness2)
}