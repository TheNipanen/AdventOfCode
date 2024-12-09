package day9

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object DiskFragmenter extends App {
  val s = Source.fromFile("src/day9/input.txt")
  val diskMap = try s.getLines().toArray.apply(0) finally s.close()
  
  val blocks = ArrayBuffer[String]()
  
  val l = diskMap.length
  var i = 0
  while (i < l) {
    val size = diskMap(i).asDigit
    val c = if (i % 2 == 0) (i / 2).toString() else "."
    blocks ++= Array.fill(size)(c)
    i += 1
  }
  
  val blocks2 = blocks.clone()
  
  i = blocks.indexWhere( _ == "." )
  var j = blocks.lastIndexWhere( _ != "." )
  while (i < j) {
    val tmp = blocks(i)
    blocks(i) = blocks(j)
    blocks(j) = tmp
    while (blocks(i) != ".") i += 1
    while (blocks(j) == ".") j -= 1
  }
  
  i = 0
  var checkSum = 0L
  while (blocks(i) != ".") {
    val id = blocks(i).toLong
    checkSum += i.toLong * id
    i += 1
  }
  
  println(checkSum)
  
  i = blocks2.lastIndexWhere( _ != "." )
  var id = blocks2(i).toInt
  while (id >= 0) {
    val lastEmpty = blocks2.lastIndexWhere(_ != id.toString(), i)
    val length = i - lastEmpty
    val free = blocks2.indexOfSlice(Array.fill(length)("."))

    if (lastEmpty != -1 && free != -1 && free < i) {
      j = i
      while (j > i - length) {
        blocks2(j) = "."
        j -= 1
      }
      j = free
      while (j < free + length) {
        blocks2(j) = id.toString()
        j += 1
      }
    }

    i = blocks2.lastIndexWhere( s => s != "." && s.toInt < id, i)
    if (i == -1) id = -1
    else id = blocks2(i).toInt
  }
  
  i = 0
  checkSum = 0L
  while (i < blocks2.length) {
    val id = blocks2(i)
    if (id != ".") {
      checkSum += i.toLong * id.toLong
    }
    i += 1
  }
  
  println(checkSum)
}