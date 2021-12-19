package day19

import scala.io.Source
import scala.collection.mutable.{ ArrayBuffer, HashMap }
import scala.math.{ Pi, cos, sin, abs }

object BeaconScanner extends App {
  val s = Source.fromFile("src/day19/input.txt")
  var lines = s.getLines().toArray
  s.close()
  val scan = ArrayBuffer[Array[String]]()
  while (lines.nonEmpty) {
    scan += lines.takeWhile(_ != "")
    lines = lines.dropWhile(_ != "").drop(1)
  }
  var scanners = scan.toArray.map(_.tail.map(r => { val row = r.split(",").map(_.toInt); (row(0), row(1), row(2)) })).map( m => (m, ArrayBuffer[(Int,Int,Int)]()) )

  def rotatePoint(rotationMatrix: Array[Array[Int]], point: (Int, Int, Int)): (Int, Int, Int) = {
    val res = Array(0, 0, 0)
    for (i <- 0 to 2) {
      res(i) = rotationMatrix(i)(0) * point._1 + rotationMatrix(i)(1) * point._2 + rotationMatrix(i)(2) * point._3
    }
    (res(0), res(1), res(2))
  }
  def matrixMultiplication(mLeft: Array[Array[Int]], mRight: Array[Array[Int]]): Array[Array[Int]] = {
    val res = Array(Array(0,0,0),Array(0,0,0),Array(0,0,0))
    for (i <- 0 to 2; j <- 0 to 2) {
      res(i)(j) = mLeft(i)(0)*mRight(0)(j) + mLeft(i)(1)*mRight(1)(j) + mLeft(i)(2)*mRight(2)(j)
    }
    return res
  }
  def matchesBeacons(scanner2: Array[(Int, Int, Int)], scanner1: Array[(Int, Int, Int)]): Option[((Int, Int, Int), Array[Array[Int]])] = {
    val orientations = ArrayBuffer[Array[Array[Int]]]()
    var m = Array(Array(1,0,0),Array(0,1,0),Array(0,0,1))
    for (rollIndex <- 0 until 6) {
      val oX = Pi / 2
      val sinX = sin(oX).round.toInt; val cosX = cos(oX).round.toInt
      val rotationMatrix = Array(Array(1,0,0),Array(0,cosX,-sinX),Array(0,sinX,cosX))
      m = matrixMultiplication(rotationMatrix, m)
      orientations += m
      for (turnIndex <- 0 until 3) {
        var oY = 0.0
        if (rollIndex % 2 == 0) oY += Pi / 2
        else oY -= Pi / 2
        val sinY = sin(oY).round.toInt; val cosY = cos(oY).round.toInt
        val rotationMatrixY = Array(Array(cosY,0,sinY),Array(0,1,0),Array(-sinY,0,cosY))
        m = matrixMultiplication(rotationMatrixY, m)
        orientations += m
      }
    }
    for (rotationMatrix <- orientations) {
      val beaconsMappingToCoords = HashMap[(Int, Int, Int), Int]()
      for (beacon1 <- scanner1) {
        for (beacon2 <- scanner2) {
          val rotatedPoint = rotatePoint(rotationMatrix, beacon2)
          val scannerX = beacon1._1 - rotatedPoint._1
          val scannerY = beacon1._2 - rotatedPoint._2
          val scannerZ = beacon1._3 - rotatedPoint._3
          if (beaconsMappingToCoords.get((scannerX, scannerY, scannerZ)).isDefined) beaconsMappingToCoords((scannerX, scannerY, scannerZ)) += 1
          else beaconsMappingToCoords((scannerX, scannerY, scannerZ)) = 1
        }
      }
      val highestMatches = beaconsMappingToCoords.maxBy(_._2)
      if (highestMatches._2 >= 12) return Some((highestMatches._1, rotationMatrix))
    }
    return None
  }

  while (scanners.size > 1) {
    var i = 0
    var finished = false
    while (!finished) {
      var j = 0
      val (scanner1, dependents1) = scanners(i)
      while (!finished && j < scanners.size) {
        if (i != j) {
          val (scanner2, dependents2) = scanners(j)
          val matched = matchesBeacons(scanner2, scanner1)
          if (matched.isDefined) {
            finished = true
            val (pos, rotation) = matched.get
            val rotatedAndMovedPoints = scanner2.map(p => { val rp = rotatePoint(rotation, p); (rp._1 + pos._1, rp._2 + pos._2, rp._3 + pos._3) })
            scanners(i) = (scanners(i)._1 ++ rotatedAndMovedPoints, dependents1 ++ dependents2.map( rotatePoint(rotation,_) ) ++ ArrayBuffer(pos))
            scanners(i) = (scanners(i)._1.distinct, scanners(i)._2)
            scanners = scanners.take(j) ++ scanners.drop(j + 1)
          }
        }
        j += 1
      }
      i += 1
    }
  }

  val nOfBeacons = scanners(0)._1.size
  println("Number of beacons: " + nOfBeacons)

  var maxDistance = 0
  val scannerBasePositions = scanners(0)._2
  for (s1 <- scannerBasePositions) {
    for (s2 <- scannerBasePositions) {
      val dist = abs(s1._1 - s2._1) + abs(s1._2 - s2._2) + abs(s1._3 - s2._3)
      if (dist > maxDistance) maxDistance = dist
    }
  }
  println("Largest distance between any two scanners: " + maxDistance)
}