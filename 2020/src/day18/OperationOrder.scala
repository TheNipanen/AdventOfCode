package day18

import scala.io.Source

object OperationOrder extends App {
  val s = Source.fromFile("src/day18/input.txt")
  val r = s.bufferedReader()
  var line = r.readLine()
  def containsParentheses(s: String): Boolean = {
    s.contains("(") || s.contains(")")
  }
  def findInnermostParentheses(s: String): String = {
    var i = 0
    var res = ""
    var leftFound = false
    var rightFound = false
    while (i < s.length()) {
      val c = s(i)
      c match {
        case '(' => {
          if (!rightFound) {
            res = "("
            leftFound = true
          } 
        }
        case ')' => {
          assert(leftFound)
          if (!rightFound) {
            res += c
            rightFound = true
          }
        }
        case _ => {
          if (leftFound && !rightFound) {
            res += c
          }
        }
      }
      i += 1
    }
    assert(res.startsWith("(") && res.endsWith(")"))
    assert(!containsParentheses(res.drop(1).dropRight(1)))
    res
  }
  def evaluate(s: String): Long = {
    assert(!containsParentheses(s))
    val sep = s.split(" ")
    var res = sep(0).toLong
    var i = 1
    while (i < sep.length) {
      val op = sep(i)
      val next = sep(i+1).toLong
      op match {
        case "*" => res *= next
        case "+" => res += next
        case _ => assert(false)
      }
      i += 2
    }
    res
  }
//  def evaluate2(s: String): Long = {
//    assert(!containsParentheses(s))
//    var ss = s
//    //println(ss)
//    while (ss.contains('+')) {
//      val i = ss.indexOf('+')
//      val left = ss.take(i-1).reverse.takeWhile( _.isDigit ).reverse
//      val right = ss.drop(i+2).takeWhile( _.isDigit )
//      val res = (left.toLong + right.toLong).toString()
//      ss = ss.replaceAllLiterally(left + " + " + right, res)
//    }
//    //println(ss)
//    assert(!ss.contains('+'))
//    val res = ss.split(" ").filter( _ != "*" ).foldLeft(1L)( _ * _.toLong )
//    res
//  }
  def evaluate3(s: String): String = {
    if (containsParentheses(s)) {
      val p = findInnermostParentheses(s)
      val res = evaluate3(p.drop(1).dropRight(1))
      return evaluate3(s.replaceAllLiterally(p, res))
    } else {
      var sep = s.split(" ")
      for (op <- Array("+","*")) {
        var i = 1
        while (i < sep.length) {
          if (sep(i) == op) {
            val left = sep(i-1).toLong
            val right = sep(i+1).toLong
            val res = op match {
              case "+" => left + right
              case "*" => left * right
              case _ => {
                assert(false)
                -1L
              }
            }
            sep = sep.take(i-1) ++ Array(res.toString) ++ sep.drop(i+2)
          } else {
            i += 2
          }
        }
      }
      assert(sep.length == 1)
      return sep(0)
    }
  }
  
  var sum = 0L
  var sum2 = 0L
  while (line != null) {
    //println(line)
    var line2 = line
    //val line3 = line
    //println(line)
    while (containsParentheses(line)) {
      val p = findInnermostParentheses(line)
      val e = evaluate(p.drop(1).dropRight(1)).toString()
      line = line.replaceAllLiterally(p, e)
    }
//    while (containsParentheses(line2)) {
//      val p = findInnermostParentheses(line2)
//      val e = evaluate2(p.drop(1).dropRight(1)).toString()
//      line2 = line2.replaceAllLiterally(p, e)
//    }
    //println(line)
    //println(line2)
    assert(!containsParentheses(line))
    //assert(!containsParentheses(line2))
    val res = evaluate(line)
    //println(res)
    sum += res
    //val res2 = evaluate2(line2)
    //println(res2)
    //sum2 += res2
    val res3 = evaluate3(line2).toLong
    sum2 += res3
    //println(res3)
//    if (res2 != res3) {
//      println("false")
//      println(line3)
//      println(res2)
//      println(res3)
//    }
    line = r.readLine()
  }
  println("Sum of results: " + sum)
  println("Sum of results 2: " + sum2)
}