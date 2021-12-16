package day16

import scala.io.Source
import java.math.BigInteger

object PacketDecoder extends App {
  val s = Source.fromFile("src/day16/input.txt")
  val lineHex = s.getLines().toArray.apply(0)
  s.close()
  val line = (new BigInteger(lineHex, 16)).toString(2)

  def resValue(typeID: String): BigInteger = {
    typeID match {
      case "000" => BigInteger.ZERO
      case "001" => BigInteger.ONE
      case "010" => new BigInteger("-1")
      case "011" => BigInteger.ZERO
      case _     => BigInteger.ZERO
    }
  }

  var versionSum = 0
  def parsePackage(remaining: String): (BigInteger, String) = {
    var r = remaining
    versionSum += Integer.parseInt(r.take(3), 2)
    r = r.drop(3)
    val typeID = r.take(3)
    r = r.drop(3)

    if (typeID == "100") {
      var end = false
      var number = new StringBuilder("")
      while (!end) {
        val part = r.take(5)
        r = r.drop(5)
        if (part.head == '0') end = true
        number.append(part.tail)
      }
      val n = new BigInteger(number.dropWhile(_ == '0').toString, 2)
      return (n, r)
    }

    val lTypeID = r.head
    r = r.tail
    if (lTypeID == '0') {
      val subLength = Integer.parseInt(r.take(15).dropWhile(_ == '0'), 2)
      r = r.drop(15)
      var rem = r.take(subLength)
      r = r.drop(subLength)
      var res = resValue(typeID)
      while (rem != "") {
        val (n, remm) = parsePackage(rem)
        rem = remm
        typeID match {
          case "000" => res = res.add(n)
          case "001" => res = res.multiply(n)
          case "010" => if (res.equals(new BigInteger("-1"))) res = n else res = res.min(n)
          case "011" => res = res.max(n)
          case "101" => {
            val (nn, remmm) = parsePackage(remm)
            if (n.compareTo(nn) > 0) res = BigInteger.ONE
            rem = remmm
          }
          case "110" => {
            val (nn, remmm) = parsePackage(remm)
            if (n.compareTo(nn) < 0) res = BigInteger.ONE
            rem = remmm
          }
          case "111" => {
            val (nn, remmm) = parsePackage(remm)
            if (n.compareTo(nn) == 0) res = BigInteger.ONE
            rem = remmm
          }
        }
      }
      return (res, r)
    }

    val subLength = Integer.parseInt(r.take(11).dropWhile(_ == '0'), 2)
    r = r.drop(11)
    var res = resValue(typeID)
    var i = 0
    while (i < subLength) {
      val (n, rr) = parsePackage(r)
      r = rr
      typeID match {
        case "000" => res = res.add(n)
        case "001" => res = res.multiply(n)
        case "010" => if (res.equals(new BigInteger("-1"))) res = n else res = res.min(n)
        case "011" => res = res.max(n)
        case "101" => {
          val (nn, remmm) = parsePackage(rr)
          i += 1
          if (n.compareTo(nn) > 0) res = BigInteger.ONE
          r = remmm
        }
        case "110" => {
          val (nn, remmm) = parsePackage(rr)
          i += 1
          if (n.compareTo(nn) < 0) res = BigInteger.ONE
          r = remmm
        }
        case "111" => {
          val (nn, remmm) = parsePackage(rr)
          i += 1
          if (n.compareTo(nn) == 0) res = BigInteger.ONE
          r = remmm
        }
      }
      i += 1
    }
    return (res, r)
  }

  val (eval, r) = parsePackage(line)
  println("Sum of version numbers: " + versionSum)
  println("Result of evaluating the expression: " + eval)
}