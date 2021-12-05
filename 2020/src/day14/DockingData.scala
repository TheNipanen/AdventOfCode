package day14

import scala.io.Source

object DockingData extends App {
  val s = Source.fromFile("src/day14/input.txt")
  val reader = s.bufferedReader()
  var line = reader.readLine()
  val a, mem = scala.collection.mutable.HashMap[Int, Long]()
  def parseMask(mask: String) = {
    a.clear()
    mask.toCharArray().reverse.zipWithIndex.filter( _._1 != 'X' ).foreach({ case (bit, i) => a(i) = bit.asDigit.toLong })
  }
  def applyMask(n: Long): Long = {
    var res = n
    for ((i, bit) <- a) {
      val maskPart = 1L << i
      //println(bit)
      res = bit match {
        case 1L => res | maskPart
        case 0L => res & (maskPart ^ (-1L))
      }
    }
    res
  }
  
  while (line != null) {
    val spl = line.split("=")
    val instr = spl(0).dropRight(1)
    val data = spl(1).drop(1)
    if (instr == "mask") parseMask(data)
    else {
      val addr = instr.dropWhile( !_.isDigit ).takeWhile( _.isDigit ).toInt
      val n = data.toLong
      mem(addr) = applyMask(n)
    }
    line = reader.readLine()
  }
  s.close()
  val sum = mem.values.sum
  println("Sum of memory values: " + sum)
  
  // Part one
  //=====================
  // Part two
  
  val ss = Source.fromFile("src/day14/input.txt")
  val r = ss.bufferedReader()
  line = r.readLine()
  var mask = ""
  def parseMask2(m: String) = {
    mask = m
  }
  val memory = scala.collection.mutable.HashMap[Long, Long]()
  def writeToMemory(addr: Long, value: Long) = {
    val orig = (("0" * 36) + addr.toBinaryString).takeRight(36)
    //println(orig)
    val addresses = scala.collection.mutable.ArrayBuffer[String]("")
    var i = 0
    val l = orig.length
    while (i < l) {
      val prev = addresses.clone()
      val bit = mask(i)
      for (address <- prev) {
        bit match {
          case '1' => addresses += (address + bit)
          case '0' => addresses += (address + orig(i))
          case 'X' => {
            addresses += (address + '1')
            addresses += (address + '0')
          }
        }
        addresses -= address
      }
      i += 1
    }
    def sToL(str: String): Long = {
      var i = 0
      var res = 0L
      while (i < str.length()) {
        val bit = str(i).asDigit.toLong
        res = (res << 1) + bit
        i += 1
      }
      res
    }
    
    //println(addresses)
    val memAddresses = addresses.map( sToL(_) )
    //println(memAddresses)
    memAddresses.foreach( memory(_) = value )
  }
  
  while (line != null) {
    val spl = line.split("=")
    val instr = spl(0).dropRight(1)
    val data = spl(1).drop(1)
    if (instr == "mask") parseMask2(data)
    else {
      val addr = instr.dropWhile( !_.isDigit ).takeWhile( _.isDigit ).toLong
      val n = data.toLong
      writeToMemory(addr, n)
    }
    line = r.readLine()
  }
  ss.close()
  val sum2 = memory.values.sum
  println("Sum of memory values again: " + sum2)
}