package day7

import scala.io.Source

object NoSpaceLeftOnDevice extends App {
  val s = Source.fromFile("src/day7/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  var i = 0
  val dirSize = scala.collection.mutable.Map[String, Long]()
  var currentDir = ""
  
  while (i < lines.length) {
    val line = lines(i)
    val parts = line.split(" ")
    parts(0) match {
      case "$" => {
        if (parts(1) == "cd") {
          if (parts(2) != "..") {
            if (currentDir != "") {
              currentDir += "\\" + parts(2)
            } else {
              currentDir += parts(2)
            }
          } else {
            currentDir = currentDir.reverse.dropWhile( _ != '\\' ).drop(1).reverse
          }
        }
        i += 1
      }
      case _ => {
        var sum = 0L
        while (i < lines.length && lines(i)(0) != '$') {
          val currentParts = lines(i).split(" ")
          if (currentParts(0) != "dir") {
            sum += currentParts(0).toLong
          }
          i += 1
        }
        var dir = currentDir
        dirSize(currentDir) = 0
        while (dir != "") {
          dirSize(dir) += sum
          dir = dir.reverse.dropWhile( _ != '\\' ).drop(1).reverse
        }
      }
    }
  }
  
  val resultSum = dirSize.filter( _._2 <= 100000 ).map( _._2 ).sum
  println("Sum of directory sizes of at most 100000: " + resultSum)
  
  val totalUsed = dirSize("/")
  println("Total space used: " + totalUsed)
  val free = 70000000L - totalUsed
  println("Free space: " + free)
  val needToDelete = 30000000L - free
  println("Need to delete: " + needToDelete)
  
  val largeEnough = dirSize.filter( _._2 >= needToDelete )
  val smallest = largeEnough.minBy( _._2 )
  println("Delete directory " + smallest._1 + " with size " + smallest._2)
}