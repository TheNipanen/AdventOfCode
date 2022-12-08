package day8

import scala.io.Source

object TreetopTreeHouse extends App {
  val s = Source.fromFile("src/day8/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  def isVisibleInDir(di: Int, dj: Int, i: Int, j: Int): Boolean = {
    val l = lines(i)(j).asDigit
    var ii = i + di
    var jj = j + dj
    while (ii >= 0 && jj >= 0 && ii < lines.length && jj < lines(i).length) {
      if (lines(ii)(jj).asDigit >= l) {
        return false
      }
      ii += di
      jj += dj
    }
    return true
  }
  def isVisible(i: Int, j: Int): Boolean = {
    if (isVisibleInDir(1, 0, i, j)) return true
    if (isVisibleInDir(-1, 0, i, j)) return true
    if (isVisibleInDir(0, 1, i ,j)) return true
    return isVisibleInDir(0, -1, i ,j)
  }
  
  def scenicScoreInDir(di: Int, dj: Int, i: Int, j: Int): Int = {
    val l = lines(i)(j).asDigit
    var ii = i + di
    var jj = j + dj
    var score = 0
    while (ii >= 0 && jj >= 0 && ii < lines.length && jj < lines(i).length)  {
      score += 1
      if (lines(ii)(jj).asDigit >= l) {
        ii = -1
        jj = lines(i).length
      }
      ii += di
      jj += dj
    }
    return score
  }
  def scenicScore(i: Int, j: Int) = {
    val ss = scenicScoreInDir(1, 0, i, j)
    val sn = scenicScoreInDir(-1, 0, i ,j)
    val se = scenicScoreInDir(0, 1, i, j)
    val sw = scenicScoreInDir(0, -1, i, j)
    ss * sn * se * sw
  }
  
  var visible = 0
  var i = 0
  var maxScenicScore = 0
  while (i < lines.length) {
    val line = lines(i)
    var j = 0
    while (j < line.length) {
      if (i == 0 || j == 0 || i == lines.length - 1 || j == line.length - 1 || isVisible(i, j)) {
        visible += 1
      }
      maxScenicScore = maxScenicScore max scenicScore(i, j)
      j += 1
    }
    i += 1
  }
  
  println("Visible trees: " + visible)
  println("Maximum scenic score: " + maxScenicScore)
}