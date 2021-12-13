package day13

import scala.io.Source

object TransparentOrigami extends App {
  val s = Source.fromFile("src/day13/input.txt")
  val lines = s.getLines().toArray
  s.close()
  val coords = lines.takeWhile( _ != "" ).map( _.split(",").map( _.toInt ) )
  
  val folds = lines.dropWhile( _ != "" ).drop(1).map( _.dropWhile( c => c != 'x' && c != 'y' ).split("=") )
  var paper = Array.ofDim[Boolean](folds.find( _(0) == "x" ).get(1).toInt * 2 + 1, folds.find( _(0) == "y" ).get(1).toInt * 2 + 1)
  for (c <- coords) {
    paper(c(0))(c(1)) = true
  }
  
  def foldUp(line: Int) = {
    val nextPaper = Array.ofDim[Boolean](paper.size, line)
    for (currentLine <- 0 until line) {
      for (x <- 0 until paper.size) {
        nextPaper(x)(currentLine) = paper(x)(currentLine) || paper(x)(line + line - currentLine)
      }
    }
    paper = nextPaper
  }
  
  def foldLeft(column: Int) = {
    val nextPaper = Array.ofDim[Boolean](column, paper(0).size)
    for (currentColumn <- 0 until column) {
      for (y <- 0 until paper(0).size) {
        nextPaper(currentColumn)(y) = paper(currentColumn)(y) || paper(column + column - currentColumn)(y)
      }
    }
    paper = nextPaper
  }
  
  folds(0)(0) match {
    case "x" => foldLeft(folds(0)(1).toInt)
    case "y" => foldUp(folds(0)(1).toInt)
  }
  val visibleDots = paper.flatten.count( c => c )
  println("Visible dots after first fold: " + visibleDots)
  
  for (i <- 1 until folds.size) {
    folds(i)(0) match {
      case "x" => foldLeft(folds(i)(1).toInt)
      case "y" => foldUp(folds(i)(1).toInt)
    }
  }
  
  println("Code: ")
  for (y <- 0 until paper(0).size) {
    for (x <- 0 until paper.size) {
      print(if (paper(x)(y)) "#" else ".")
    }
    println()
  }
}