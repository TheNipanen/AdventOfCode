package day21

object DiracDice extends App {
  val pos = Array(10, 6)
  val score = Array(0, 0)
  var dieRolled = 0
  def rollDie(): Int = {
    dieRolled += 1
    dieRolled
  }
  
  var roller = 0
  while (score(0) < 1000 && score(1) < 1000) {
    val sum = rollDie() + rollDie() + rollDie()
    pos(roller) += sum
    pos(roller) %= 10
    if (pos(roller) == 0) pos(roller) = 10
    score(roller) += pos(roller)
    roller = 1 - roller
  }
  
  val losingScore = if (score(0) < 1000) score(0) else score(1)
  println("Losing score * die rolls: " + (losingScore * dieRolled))
  
  def diracRound(pos: Array[Int], score: Array[Int], roller: Int): (Long, Long) = {
    if (score(0) >= 21) return (1L, 0L)
    if (score(1) >= 21) return (0L, 1L)
    
    var wins1 = 0L
    var wins2 = 0L
    for (i <- 3 to 9) {
      val nextPos = pos.clone()
      nextPos(roller) += i
      nextPos(roller) %= 10
      if (nextPos(roller) == 0) nextPos(roller) = 10
      val nextScore = score.clone()
      nextScore(roller) += nextPos(roller)
      val (w1, w2) = diracRound(nextPos, nextScore, 1 - roller)
      val multiplier = i match {
        case 3 => 1L
        case 4 => 3L
        case 5 => 6L
        case 6 => 7L
        case 7 => 6L
        case 8 => 3L
        case 9 => 1L
      }
      wins1 += w1 * multiplier
      wins2 += w2 * multiplier
    }
    return (wins1, wins2)
  }
  def diracGame(): (Long, Long) = {
    pos(0) = 10
    pos(1) = 6
    score(0) = 0
    score(1) = 0
    return diracRound(pos,score,0)
  }
  
  val (wins1, wins2) = diracGame()
  println("Player 1 wins in " + wins1 + " universes")
  println("Player 2 wins in " + wins2 + " universes")
}