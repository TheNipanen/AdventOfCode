package day17

object TrickShot extends App {
  val xMin = 81
  val xMax = 129
  val yMin = -150
  val yMax = -108

  def highestYPos(initialXSpeed: Int, initialYSpeed: Int): (Boolean, Int) = {
    var x = 0
    var y = 0
    var xSpeed = initialXSpeed
    var ySpeed = initialYSpeed
    var highestY = Int.MinValue
    while (!(x >= xMin && x <= xMax) || !(y >= yMin && y <= yMax)) {
      x += xSpeed
      y += ySpeed
      if (xSpeed > 0) xSpeed -= 1
      ySpeed -= 1
      if (y > highestY) highestY = y
      if (x > xMax || (y < yMin && ySpeed <= 0)) return (false, 0)
    }
    return (true, highestY)
  }
  
  var highestY = Int.MinValue
  var nOfVelocities = 0
  for (initialX <- (1000 to 0 by -1); initialY <- (1000 to -1000 by -1)) {
    val (res, y) = highestYPos(initialX, initialY)
    if (res && y > highestY) highestY = y
    if (res) nOfVelocities += 1
  }
  
  println("Highest y reached: " + highestY)
  println("Number of distinct initial velocity values getting the probe to the target area: " + nOfVelocities)
}