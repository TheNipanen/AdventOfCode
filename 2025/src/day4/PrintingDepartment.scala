package day4
import scala.io.Source

object PrintingDepartment extends App {
  val s = Source.fromFile("src/day4/input.txt")
  val grid = try s.getLines().toArray finally s.close()
  
  var accessibleRolls = 0
  
  val length = grid.size
  val width = grid(0).size
  
  def isAccessibleRoll(y: Int, x: Int): Boolean = {
    if (grid(y)(x) != '@') {
      return false
    }
    
    val neighbors =  (for (
        yy <- y - 1 to y + 1;
        xx <- x - 1 to x + 1 
        if yy >= 0 && yy < length && xx >= 0 && xx < width && (yy !=y || xx != x)
    ) yield (yy, xx)).toArray
    val accessible = neighbors.count({case (yy, xx) => grid(yy)(xx) == '@'}) < 4
    
    return accessible
  }
  
  var y = 0
  while (y < length) {
    var x = 0
    while (x < width) {
      if (isAccessibleRoll(y, x)) {
        accessibleRolls += 1
      }
      x += 1
    }
    y += 1
  }
  
  println(accessibleRolls)
  
  def removeRound(): Int = {
    var removedRolls = 0
    var y = 0
    while (y < length) {
      var x = 0
      while (x < width) {
        if (isAccessibleRoll(y, x)) {
          grid(y) = grid(y).updated(x, 'X')
          removedRolls += 1
        }
        x += 1
      }
      y += 1
    }
    return removedRolls
  }
  
  var removedRolls = 0
  var removedThisRound = 0
  while ({removedThisRound = removeRound(); removedThisRound > 0}) {
    removedRolls += removedThisRound
  }
  
  println(removedRolls)
}