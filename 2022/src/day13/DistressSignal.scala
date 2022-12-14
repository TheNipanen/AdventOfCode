package day13

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object DistressSignal extends App {
  val s = Source.fromFile("src/day13/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  val packets = lines.filter( _ != "" ).map( parsePacket(_) )
  
  def parsePacket(line: String): Array[Any] = {
    val result = ArrayBuffer[Any]()
    var i = 1
    val l = line.length
    while (i < l) {
      val c = line(i)
      if (c.isDigit) {
        var n = ""
        while (line(i).isDigit) {
          n += line(i)
          i += 1
        }
        result += n.toInt
      } else if (c == '[') {
        var inner = "["
        var count = 1
        i += 1
        while (count > 0) {
          val char = line(i)
          inner += char
          if (char == '[') count += 1
          else if (char == ']') count -= 1
          i += 1
        }
        result += parsePacket(inner)
      }
      i += 1
    }
    result.toArray
  }
  
  def compare(left: Any, right: Any): Option[Boolean] = {
    (left, right) match {
      case (l: Int, r: Int) => {
        if (l == r) return None
        return Some(l < r)
      }
      case (l: Array[Any], r: Array[Any]) => {
        val ll = l.length
        val lr = r.length
        var i = 0
        while (i < ll && i < lr) {
          val result = compare(l(i), r(i))
          if (result.isDefined) return result
          i += 1
        }
        if (ll == lr) return None
        return Some(ll < lr)
      }
      case (l: Array[Any], r: Int) => {
        return compare(l, Array[Any](r))
      }
      case (l: Int, r: Array[Any]) => {
        return compare(Array[Any](l), r)
      }
      case _ => {
        throw new Exception("Bad comparison")
      }
    }
  }
  
  val pairs = packets.grouped(2).toArray
  var i = 0
  var sum = 0
  while (i < pairs.length) {
    val pair = pairs(i)
    val result = compare(pair(0), pair(1))
    if (result.isDefined && result.get) {
      sum += i + 1
    }
    i += 1
  }
  
  println("Sum of the indices of right-ordered pairs: " + sum)
  
  val newPackets = packets ++ Array[Any](parsePacket("[[2]]"), parsePacket("[[6]]"))
  val sorted = newPackets.sortWith( (l, r) => {
    val res = compare(l, r)
    res.get
  })
  
  def getIndex(int: Int): Int = {
    var ind = 0
    while (ind < sorted.length) {
      val packet = sorted(ind)
      packet match {
        case p: Array[Any] => {
          if (p.length == 1) {
            val inner = p(0)
            inner match {
              case i: Array[Any] => {
                if (i.length == 1 && i(0) == int) return ind + 1
              }
              case _ => {}
            }
          }
        }
        case _ => {}
      }
      ind += 1
    }
    return -1
  }
  
  val di1 = getIndex(2)
  val di2 = getIndex(6)
  val decoderKey = di1 * di2
  println("Decoder key: " + decoderKey)
}