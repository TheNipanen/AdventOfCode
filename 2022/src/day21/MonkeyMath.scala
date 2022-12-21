package day21

import scala.io.Source
import scala.collection.mutable.HashMap

object MonkeyMath extends App {
  val s = Source.fromFile("src/day21/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  class Monkey(var n: Option[Long], val lChild: Option[String], val rChild: Option[String], val op: Option[String], val name: String) {
    def number(): Long = {
      if (n.isDefined) {
        return n.get
      }
      val ln = monkeys(lChild.get).number()
      val rn = monkeys(rChild.get).number()
      val nn = op.get match {
        case "+" => ln + rn
        case "-" => ln - rn
        case "*" => ln * rn
        case "/" => ln / rn
      }
      return nn
    }
    
    def solve(): String = {
      if (name == "humn") {
        return "x"
      }
      if (this.containsHumn()) {
        return  "(" + monkeys(lChild.get).solve() + op.get + monkeys(rChild.get).solve() + ")" 
      }
      return this.number().toString
    }
    
    def containsHumn(): Boolean = {
      name == "humn" || (lChild.isDefined && (monkeys(lChild.get).containsHumn() || monkeys(rChild.get).containsHumn()))
    }
  }
  val monkeys = HashMap[String, Monkey]()
  val parent = HashMap[String, String]()
  
  var i = 0
  while (i < lines.length) {
    val parts = lines(i).split(" ")
    val name = parts(0).dropRight(1)
    if (parts(1)(0).isDigit) {
      monkeys(name) = new Monkey(Some(parts(1).toLong), None, None, None, name)
    } else {
      monkeys(name) = new Monkey(None, Some(parts(1)), Some(parts(3)), Some(parts(2)), name)
      parent(parts(1)) = name
      parent(parts(3)) = name
    }
    i += 1
  }
  
  val root = monkeys("root")
  val rootN = root.number()
  println("Monkey named root will yell: " + rootN)
  
  val humn = monkeys("humn")
  val l = monkeys(root.lChild.get)
  val r = monkeys(root.rChild.get)
  
  // Use these if you want to print out the equation for both numbers
  //println(l.solve())
  //println(r.solve())
  
  val l1 = l.number()
  val r1 = r.number()
  humn.n = Some(humn.n.get + 1000000L)
  val l2 = l.number()
  val r2 = r.number()
  
  val goal = if (l1 == l2) l.number() else r.number()
  val change = if (l1 == l2) r else l
  
  var start = -100000000000000000L
  var end = 100000000000000000L
  
  // Determine if the plot of the equation is rising
  humn.n = Some(start)
  val ss = change.number()
  humn.n = Some(end)
  val ee = change.number()
  val rising = ss < ee
  
  // Solve linear equation with binary search
  while (start <= end) {
    val mid = (start + end) / 2L
    humn.n = Some(mid)
    val comp = change.number()
    if (rising && comp > goal) {
      end = mid - 1L
    } else if (rising && comp < goal) {
      start = mid + 1L
    } else if (comp < goal) {
      end = mid - 1L
    } else if (comp > goal) {
      start = mid + 1L
    } else {
      start = mid
      end = mid - 1L
    }
  }
  
  println("Number to yell: " + start)
}