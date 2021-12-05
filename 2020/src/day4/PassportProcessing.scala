package day4

import scala.io.Source
import scala.Vector

object PassportProcessing extends App {
  val reqFields = Vector("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
  val s = Source.fromFile("src/day4/input.txt")
  val reader = s.bufferedReader()
  
  val ecl = Vector("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  var valid = 0
  var line = reader.readLine()
  while (line != null) {
//    println("Loop start")
    var s = ""
    while (line != "" && line != null) {
//      println("Inner: " + line)
      s += line + " "
      line = reader.readLine()
    }
    val fields = s.split(" ").map( _.takeWhile( _ != ':' ) )
    var validPP = true
    for (field <- reqFields if (validPP)) {
      if (fields.contains(field)) {
        val ss = s.drop(s.indexOf(field))
        val value = ss.drop(4).takeWhile( _ != ' ' )
        field match {
          case "byr" => {
            val f = value.toInt
            if (value.length() != 4 || f < 1920 || f > 2002) validPP = false
          }
          case "iyr" => {
            val f = value.toInt
            if (value.length() != 4 || f < 2010 || f > 2020) validPP = false
          }
          case "eyr" => {
            val f = value.toInt
            if (value.length() != 4 || f < 2020 || f > 2030) validPP = false
          }
          case "hgt" => {
            val n = value.takeWhile( _.isDigit ).toInt
            val unit = value.dropWhile( _.isDigit )
            if (unit != "cm" && unit != "in") validPP = false 
            else {
              if (unit == "cm") {
                if (n < 150 || n > 193) validPP = false
              } else {
                if (n < 59 || n > 76) validPP = false
              }
            }
          }
          case "hcl" => {
            if (value.length() != 7 || value(0) != '#') validPP = false
            else {
              var i = 1
              while (i < value.length() && validPP) {
                val c = value(i)
                if (!c.isDigit && (c < 'a' || c > 'f')) validPP = false
                i += 1
              }
            }
          }
          case "ecl" => {
            if (!ecl.contains(value)) validPP = false
          }
          case "pid" => {
            if (value.length() != 9 || !value.forall( _.isDigit )) validPP = false
          }
        }
      } else {
        validPP = false
      }
    }
    if (validPP) valid += 1
    line = reader.readLine()
  }
  s.close()
  println("Valid passports: " + valid)
}