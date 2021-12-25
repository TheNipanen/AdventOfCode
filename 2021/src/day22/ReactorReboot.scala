package day22

import scala.io.Source
import scala.collection.mutable.{ HashSet, HashMap }

object ReactorReboot extends App {
  val s = Source.fromFile("src/day22/input.txt")
  val instructions = s.getLines().toArray.map( l => {val s = l.split(" "); val i = s(1).split(",").map(_.drop(2).split("\\.\\.")); (s(0),
      (i(0)(0).toInt,i(0)(1).toInt), 
      (i(1)(0).toInt,i(1)(1).toInt), 
      (i(2)(0).toInt,i(2)(1).toInt))} )
  s.close()
  
  val on = HashSet[(Int,Int,Int)]()
  def instruction(ins: String, x1: Int, x2: Int, y1: Int, y2: Int, z1: Int, z2: Int) = {
    val xx1 = x1 max -5
    val xx2 = x2 min 50
    val yy1 = y1 max -50
    val yy2 = y2 min 50
    val zz1 = z1 max -50
    val zz2 = z2 min 50
    ins match {
      case "on" => {
        for (x <- xx1 to xx2; y <- yy1 to yy2; z <- zz1 to zz2) {
          on((x,y,z)) = true
        }
      }
      case "off" => {
        for ((x,y,z) <- on) {
          if (x >= xx1 && x <= xx2 && y >= yy1 && y <= yy2 && z >= zz1 && z <= zz2) {
            on((x,y,z)) = false
          }
        }
      }
    }
  }
  
  var i = 0
  while (i < instructions.size) {
    val (ins, (x1,x2), (y1,y2), (z1,z2)) = instructions(i)
    instruction(ins, x1, x2, y1, y2, z1, z2)
    i += 1
  }
  
  val onCubes = on.size
  println("Cubes on in the region: " + onCubes)
  
  val allCuboids = HashMap[(Long, Long, Long, Long, Long, Long), Long]()
  for ((ins, (x1,x2), (y1,y2), ( z1,z2)) <- instructions) {
    val sign = if (ins == "on") 1L else -1L
    val cuboid: (Long,Long,Long,Long,Long,Long) = (x1,x2,y1,y2,z1,z2)
    val newCuboids = HashMap[(Long, Long, Long, Long, Long, Long), Long]()
    
    for (((cx1,cx2,cy1,cy2,cz1,cz2),cSign) <- allCuboids) {
      val ix1 = x1.toLong max cx1; val ix2 = x2.toLong min cx2
      val iy1 = y1.toLong max cy1; val iy2 = y2.toLong min cy2
      val iz1 = z1.toLong max cz1; val iz2 = z2.toLong min cz2
      val intersection = (ix1,ix2,iy1,iy2,iz1,iz2)
      if (ix1 <= ix2 && iy1 <= iy2 && iz1 <= iz2) {
        newCuboids(intersection) = newCuboids.getOrElse(intersection, 0L) - cSign
      }
    }
    
    if (sign == 1L) {
      newCuboids(cuboid) = newCuboids.getOrElse(cuboid, 0L) + sign
    }
    
    for ((newCube, nSign) <- newCuboids) {
      allCuboids(newCube) = allCuboids.getOrElse(newCube, 0L) + nSign
    }
  }
  
  val onCubes2 = allCuboids.foldLeft(0L)( (prev, next) => {val (c,s) = next; prev + (c._2 - c._1 + 1L) * (c._4 - c._3 + 1L) * (c._6 - c._5 + 1L) * s})
  println("Cubes on altogether: " + onCubes2)
}