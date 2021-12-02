package day19

import scala.io.Source

object MonsterMessages extends App {
  val s = Source.fromFile("src/day19/input.txt")
  val r = s.bufferedReader()
  var line = r.readLine()
  val rules = scala.collection.mutable.HashMap[Int, String]()
  //rules(0) = "b"
  //println(matchesRule(0,"b"))
  while (line != "") {
    val (n, rule) = line.splitAt(line.indexOf(':'))
    rules(n.toInt) = rule.drop(2).filter( _ != '"' )
    line = r.readLine()
  }
  rules(8) = "42 | 42 8"
  rules(11) = "42 31 | 42 11 31"
  //println(rules)
  
//  def matchesRule(n: Int, s: String, counter: Int): Option[String] = {
//    val rule = rules(n)
//    if (counter == 1000) return Some(s)
//    if (s.length == 0) return None
//    if (rule.length == 1 && !rule(0).isDigit) {
//      val c = rule(0)
//      if (s(0) == c) return Some(s.drop(1))
//      else return None
//    } else {
//      val options = rule.split('|').map( _.split(" ").filter( _ != "" ) )
//      for (ruleList <- options) {
//        var remaining: Option[String] = Some(s)
//        var matching = true
//        for (ruleNumber <- ruleList; if (matching)) {
//          if (remaining.isDefined && remaining.get.length == 0) matching = false
//          remaining = matchesRule(ruleNumber.toInt, remaining.get, counter + 1)
//          if (remaining.isEmpty) matching = false
//        }
//        if (remaining.isDefined && matching) return remaining
//      }
//      return None
//    }
//  }
  val depth = 25
//  def inner(n: Int, counter: Int): Array[String] = {
//    //if (counter == depth) return Array("")
//    val rule = rules(n)
//    if (rule.length == 1 && !rule(0).isDigit) {
//      val c = rule(0)
//      return Array("" + c)
//    } else {
//      val res = scala.collection.mutable.ArrayBuffer[String]()
//      val options = rule.split('|').map( _.split(" ").filter( _ != "" ) )
//      for (ruleList <- options) {
//        var current = Array[String]("")
//        for (ruleNumber <- ruleList) {
//          val suffixes = inner(ruleNumber.toInt, counter + 1)
//          val next = scala.collection.mutable.ArrayBuffer[String]()
//          current.foreach( c => suffixes.foreach( s => next += (c+s) ) )
//          current = next.toArray
//        }
//        res ++= current
//      }
//      return res.toArray
//    }
//  }
//  val m42 = inner(42, 0)
//  val m42L = m42(0).length
//  m42.foreach(println(_))
//  println()
//  val m31 = inner(31, 0)
//  val m31L = m31(0).length
//  m31.foreach(println(_))
  import scala.util.matching.Regex
  val ex = inner(0, 0).r
  def inner(n: Int, counter: Int): String = {
    if (counter == depth) return ""
    val rule = rules(n)
    if (rule.length == 1 && !rule(0).isDigit) return rule
    val options = rule.split('|').map( _.split(" ").filter( _ != "" ) )
    val mid = (for (ruleList <- options) yield (for (ruleNumber <- ruleList) yield inner(ruleNumber.toInt, counter + 1)).foldLeft("")( _ + _ )).mkString("|")
    return "(" + mid + ")"
  }
//  def count42(s: String) = {
//    var i = 0
//    var found = false
//    var count = 0
//    var res = -1
//    while (i < s.length - m42L) {
//      val ss = s.substring(i, i + m42L)
//      if (m42.contains(ss)) {
//        if (!found) res = i
//        count += 1
//        i += m42L
//        found = true
//      } else {
//        if (found) i = s.length
//        else i += 1
//      }
//    }
//    (count, res)
//  }
//  def count31(s: String) = {
//    var i = 0
//    var found = false
//    var count = 0
//    var res = -1
//    while (i < s.length - m31L) {
//      val ss = s.substring(i, i + m31L)
//      if (m31.contains(ss)) {
//        if (!found) res = i
//        count += 1
//        i += m31L
//        found = true
//      } else {
//        if (found) i = s.length
//        else i += 1
//      }
//    }
//    (count, res)
//  }
  
  line = r.readLine()
  var count = 0
  while (line != null) {
    //println(line)
    //val res = matchesRule(0, line, 0)
    //if (res.isDefined && res.get.length == 0) count += 1
//    var ind = 0
//    while (ind < 25) {
//      val (c, i) = count42(line)
//      if (c > 1) {
//        val ss = line.substring(i, i + (c * m42L))
//        line = line.replaceFirst(ss, m42(0))
//        val (c31, j) = count31(line)
//        if (c31 > 1) {
//          val sss = line.substring(j, j + (c31 * m31L))
//          line = line.replaceFirst(sss, m31(0))
//        }
//      }
//      ind += 1
//    }
    val found = ex.findFirstIn(line)
    if (found.isDefined && found.get == line) count += 1
    line = r.readLine()
  }
  println("Rules matching rule 0: " + count)
}