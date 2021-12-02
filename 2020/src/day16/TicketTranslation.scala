package day16

import scala.io.Source

object TicketTranslation extends App {
  val s = Source.fromFile("src/day16/input.txt")
  val r = s.bufferedReader()
  var line = r.readLine()
  
  val fields = scala.collection.mutable.HashMap[String, (Int,Int,Int,Int)]()
  while (line != "") {
    val (field, values) = line.splitAt(line.indexOf(':'))
    val ranges = values.drop(1).filter( _ != ' ' ).split("or").map( _.split("-") ).flatten.map( _.toInt )
    val v = ranges match { case Array(a, b, c, d) => (a,b,c,d) }
    fields(field) = v
    line = r.readLine()
  }
  
  r.readLine()
  line = r.readLine()
  val myTicket = line.split(",").map( _.toInt )
  
  def isValid(fieldVal: Int): Boolean = {
    var res = 
    for ((f,v) <- fields) {
      if ((fieldVal >= v._1 && fieldVal <= v._2) || (fieldVal >= v._3 && fieldVal <= v._4)) return true
    }
    return false
  }
  r.readLine()
  r.readLine()
  val nearbyTickets = scala.collection.mutable.ArrayBuffer[Array[Int]]()
  line = r.readLine()
  var error = 0
  var validos = 0
  while (line != null) {
    val t = line.split(",").map( _.toInt )
    var valid = true
    for (v <- t) {
      if (!isValid(v)) {
        error += v
        valid = false
      }
    }
    if (valid) {
      nearbyTickets += t
      validos += 1
    }
    line = r.readLine()
  }
  //myTicket.foreach(println(_))
  
  println("Ticket scanning error rate: " + error)
  
  val fieldNumber = scala.collection.mutable.HashMap[String, Int]()
  val tickets = Array(myTicket) ++ nearbyTickets
  //var nofFields = 1
  def matchesField(field: String, value: Int): Boolean = {
    val v = fields(field)
    if ((value >= v._1 && value <= v._2) || (value >= v._3 && value <= v._4)) true
    else false
  }
  def fieldsMatched(value: Int): Array[String] = {
    val res = scala.collection.mutable.ArrayBuffer[String]()
    for (f <- fields.keys) {
      if (matchesField(f, value)) res += f
    }
    res.toArray
  }
  def fieldsMatchedByColumn(col: Int): Array[String] = {
    var i = 0
    var matches = fields.keySet.toArray
    while (i < tickets.length) {
      val v = tickets(i)(col)
      val vMatches = fieldsMatched(v)
      matches = matches.filter( vMatches.contains(_) )
      i += 1
    }
    matches
  }
  val columnsMatchedByField = scala.collection.mutable.HashMap[String, Array[Int]]()
  for (col <- (0 until myTicket.length)) {
    val f = fieldsMatchedByColumn(col)
    f.foreach( fi => {
      if (columnsMatchedByField.contains(fi)) columnsMatchedByField(fi) = columnsMatchedByField(fi) :+ col
      else columnsMatchedByField(fi) = Array(col)
    })
  }
  val sorted = columnsMatchedByField.toArray.sortBy( _._2.length )
  var i = 0
  while (i < sorted.length) {
    val (field, columns) = sorted(i)
    val column = columns.filter( !fieldNumber.valuesIterator.contains(_) )
    assert(column.length == 1)
    fieldNumber(field) = column(0)
    i += 1
  }
  
//  val fieldsConsidered = fields.keySet.toBuffer
//  var fieldN = 0
//  while (fieldN < fields.keySet.size) {
//    var i = 0
//    var available = fieldsConsidered.clone()
//    while (i < tickets.length) {
//      val t = tickets(i)
//      val v = t(fieldN)
//      val matched = fieldsMatched(v)
//      available = available.filter(matched.contains(_))
//      if (available.length == 1) {
//        i = tickets.length
//      }
//      i += 1
//    }
//    if (available.length == 1) {
//      val field = available(0)
//      fieldsConsidered -= field
//      fieldNumber(field) = fieldN
//    }
//    fieldN += 1
//  }
//  var i = 0
//  while (i < tickets.length) {
//    val t = tickets(i)
//    var j = 0
//    while (j < t.length) {
//      val v = t(j)
//      val matched = fieldsMatched(v).filter( fieldsConsidered.contains(_) )
//      if (matched.length == 1) {
//        fieldNumber(matched(0)) = j
//      }
//      j += 1
//    }
//    i += 1
//  }
//  while (nofFields <= fields.keys.size) {
//    var found = false
//    var i = 0
//    while (i < tickets.length) {
//      val t = tickets(i)
//      var j = 0
//      while (j < t.length) {
//        val v = t(j)
//        val fields2 = fieldsMatched(v).filter( !fieldNumber.keySet.contains(_) )
//        //fields2.foreach(println(_))
//        //tln()
//        if (fields2.length == 1) {
//          println(fields2(0))
//          fieldNumber(fields2(0)) = j
//        }
//        j += 1
//      }
//      
//      
//      i += 1
//    }
//    nofFields += 1
//  }
//  
//  println(fieldNumber.keySet)
//  val matchedFields = scala.collection.mutable.HashMap[Int, List[String]]()
//  
//  var i = 0
//  while (i < tickets.length) {
//    val t = tickets(i)
//    var j = 0
//    while (j < t.length) {
//      val v = t(j)
//      val fields2 = fieldsMatched(v)
//      if (!matchedFields.keySet.contains(j)) matchedFields(j) = fields2.toList
//      else {
//        for (f <- fields2) {
//          if (!matchedFields(j).contains(f)) matchedFields(j) = matchedFields(j) :+ f
//        }
//      }
//      j += 1
//    }
//    i += 1
//  }
//  
//  def inner(fieldN: Int): Boolean = {
//    if (fieldN == fields.size) return true
//    for (f <- fields.keySet) {//matchedFields(fieldN)) {
//      if (!fieldNumber.keySet.contains(f)) {
//        fieldNumber(f) = fieldN
//        if (inner(fieldN + 1)) return true
//        fieldNumber -= f
//      }
//    }
//    return false
//  }
//  if(!inner(0)) println("false")
  
  //println(fieldNumber)
  val res = fields.keys.filter( _.startsWith("departure") ).map( fieldNumber(_) ).map( myTicket(_) ).foldLeft(1L)(_*_.toLong)
  println("Result: " + res)
}