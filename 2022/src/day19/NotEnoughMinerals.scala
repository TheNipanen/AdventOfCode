package day19

import scala.io.Source

object NotEnoughMinerals extends App {
  val s = Source.fromFile("src/day19/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  def maxGeodes(blueprint: String, part1: Boolean) = {
    val parts = blueprint.split("\\. ")
    val oreRobotCost = parts(0).reverse.drop(4).takeWhile( _.isDigit ).reverse.toInt
    val clayRobotCost = parts(1).drop(22).takeWhile( _.isDigit ).toInt
    val obsidianRobotCostOre = parts(2).drop(26).takeWhile( _.isDigit ).toInt
    val obsidianRobotCostClay = parts(2).reverse.drop(5).takeWhile( _.isDigit ).reverse.toInt
    val geodeRobotCostOre = parts(3).drop(23).takeWhile( _.isDigit ).toInt
    val geodeRobotCostObsidian = parts(3).reverse.drop(10).takeWhile( _.isDigit ).reverse.toInt
    
    val maxOreCost = oreRobotCost max clayRobotCost max obsidianRobotCostOre max geodeRobotCostOre
    val minOreCost = oreRobotCost min clayRobotCost
    
    val maxGeodes = Array.fill(if (part1) 24 else 32)(0)
    var count = 0L
    def dfs(state: ((Int, Int), (Int, Int), (Int, Int), (Int, Int)), time: Int): Unit = {
      if ((part1 && time > 24) || time > 32) {
        maxGeodes(time - 2) = maxGeodes(time - 2) max state._4._1
        count += 1L
        if (count % 1000000000L == 0) println(count)
        return
      }
      val newOre = state._1._1 + state._1._2
      val newClay = state._2._1 + state._2._2
      val newObsidian = state._3._1 + state._3._2
      val newGeodes = state._4._1 + state._4._2
      if (newGeodes < maxGeodes(time - 1)) {
        count += 1L
        return
      }
      maxGeodes(time - 1) = newGeodes
      if (state._1._1 >= geodeRobotCostOre && state._3._1 >= geodeRobotCostObsidian) {
        dfs(((newOre - geodeRobotCostOre, state._1._2), (newClay, state._2._2), (newObsidian - geodeRobotCostObsidian, state._3._2), (newGeodes, state._4._2 + 1)), time + 1)
      } else {
        if (state._1._1 >= oreRobotCost && state._1._2 < maxOreCost) {
          dfs(((newOre - oreRobotCost, state._1._2 + 1), (newClay, state._2._2), (newObsidian, state._3._2), (newGeodes, state._4._2)), time + 1)
        }
        if (state._1._1 >= clayRobotCost && state._2._2 < obsidianRobotCostClay) {
          dfs(((newOre - clayRobotCost, state._1._2), (newClay, state._2._2 + 1), (newObsidian, state._3._2), (newGeodes, state._4._2)), time + 1)
        }
        if (state._1._1 >= obsidianRobotCostOre && state._2._1 >= obsidianRobotCostClay && state._3._2 < geodeRobotCostObsidian) {
          dfs(((newOre - obsidianRobotCostOre, state._1._2), (newClay - obsidianRobotCostClay, state._2._2), (newObsidian, state._3._2 + 1), (newGeodes, state._4._2)), time + 1)
        }
        if (state._1._1 <= minOreCost) {
          dfs(((newOre, state._1._2), (newClay, state._2._2), (newObsidian, state._3._2), (newGeodes, state._4._2)), time + 1)
        }
      }
    }
    dfs(((0,1),(0,0),(0,0),(0,0)), 1)
    maxGeodes(if (part1) 23 else 31)
  }
  
  var i = 0
  var result = 0
  while (i < lines.length) {
    val geodes = maxGeodes(lines(i), true)
    i += 1
    result += geodes * i
  }
  
  println("Sum of blueprint quality levels: " + result)
  
  i = 0
  result = 1
  while (i < 3) {
    val geodes = maxGeodes(lines(i), false)
    result *= geodes
    i += 1
  }
  
  println("Three first largest geodes multiplied: " + result)
}