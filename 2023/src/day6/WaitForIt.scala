package day6

object WaitForIt extends App {
  val time = Array(48, 98, 90, 83)
  val dist = Array(390, 1103, 1112, 1360)
  
  val waysToWin = Array.fill(time.length)(0)
  
  val l = time.length
  var i = 0
  while (i < l) {
    val t = time(i)
    val record = dist(i)
    
    for (chargeTime <- 0 to t) {
      val speed = chargeTime
      val travelTime = t - chargeTime
      val d = speed * travelTime
      if (d > record) {
        waysToWin(i) += 1
      }
    }
    i += 1
  }
  
  val multiplied = waysToWin.foldLeft(1)( _ * _ )
  println("Ways to win each race multiplied together: " + multiplied)
  
  val t = time.foldLeft("")( _ + _ ).toLong
  val record = dist.foldLeft("")( _ + _ ).toLong
  var wins = 0L
  for (chargeTime <- 0L to t) {
    val speed = chargeTime
    val travelTime = t - chargeTime
    val d = speed * travelTime
    if (d > record) {
      wins += 1L
    }
  }
  
  println("Ways to win the large race: " + wins)
}