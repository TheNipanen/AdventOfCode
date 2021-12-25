package day23

import scala.collection.mutable.{ ArrayBuffer, PriorityQueue, HashMap, ListBuffer }
import scala.math.abs

object Amphipod extends App {
  val origBurrow = Array(
    Array('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'),
    Array('#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'),
    Array('#', '#', '#', 'D', '#', 'B', '#', 'C', '#', 'C', '#', '#', '#'),
    Array('#', '#', '#', 'D', '#', 'A', '#', 'B', '#', 'A', '#', '#', '#'),
    Array('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'))

  val origAmber = Array((3, 5), (3, 9))
  val origBronze = Array((2, 5), (3, 7))
  val origCopper = Array((2, 7), (2, 9))
  val origDesert = Array((2, 3), (3, 3))
  val aEntrance = (1, 3); val aGoal1 = (2, 3); val aGoal2 = (3, 3); val aGoal3 = (4, 3); val aGoal4 = (5, 3)
  val bEntrance = (1, 5); val bGoal1 = (2, 5); val bGoal2 = (3, 5); val bGoal3 = (4, 5); val bGoal4 = (5, 5)
  val cEntrance = (1, 7); val cGoal1 = (2, 7); val cGoal2 = (3, 7); val cGoal3 = (4, 7); val cGoal4 = (5, 7)
  val dEntrance = (1, 9); val dGoal1 = (2, 9); val dGoal2 = (3, 9); val dGoal3 = (4, 9); val dGoal4 = (5, 9)

  val positions = (1 to 11).map((1, _)) ++ (3 to 9 by 2).map(y => Array((2, y), (3, y), (4, y), (5, y))).flatten

  class State(val burrow: Array[Array[Char]], val amber: Vector[(Int, Int)], val bronze: Vector[(Int, Int)], val copper: Vector[(Int, Int)], val desert: Vector[(Int, Int)], val previousMover: (Int, Int), val first: Boolean) {
    def move(c: Char, i: Int, pos: (Int, Int)) = {
      val typeArray = c match {
        case 'A' => amber
        case 'B' => bronze
        case 'C' => copper
        case 'D' => desert
      }
      val (x, y) = typeArray(i)
      val newTypeArray = typeArray.updated(i, pos)
      val newBurrow = burrow.clone().map(_.clone())
      newBurrow(x)(y) = '.'
      newBurrow(pos._1)(pos._2) = c
      val newPreviousMover = pos
      new State(newBurrow, if (c == 'A') newTypeArray else amber, if (c == 'B') newTypeArray else bronze, if (c == 'C') newTypeArray else copper, if (c == 'D') newTypeArray else desert, newPreviousMover, this.first)
    }

    //  def unMove() = {
    //    val (c, i, from, to) = moves.pop()
    //    val typeArray = c match {
    //      case 'A' => amber
    //      case 'B' => bronze
    //      case 'C' => copper
    //      case 'D' => desert
    //    }
    //    val pos = typeArray(i)
    //    assert(pos == to)
    //    typeArray(i) = from
    //    burrow(pos._1)(pos._2) = '.'
    //    burrow(from._1)(from._2) = c
    //  }

    def isOccupied(pos: (Int, Int)) = burrow(pos._1)(pos._2) != '.'

    def isDone() = {
      if (this.first) burrow(aGoal1._1)(aGoal1._2) == 'A' &&
        burrow(aGoal2._1)(aGoal2._2) == 'A' &&
        burrow(bGoal1._1)(bGoal1._2) == 'B' &&
        burrow(bGoal2._1)(bGoal2._2) == 'B' &&
        burrow(cGoal1._1)(cGoal1._2) == 'C' &&
        burrow(cGoal2._1)(cGoal2._2) == 'C' &&
        burrow(dGoal1._1)(dGoal1._2) == 'D' &&
        burrow(dGoal2._1)(dGoal2._2) == 'D'
      else burrow(aGoal1._1)(aGoal1._2) == 'A' &&
        burrow(aGoal2._1)(aGoal2._2) == 'A' &&
        burrow(aGoal3._1)(aGoal3._2) == 'A' &&
        burrow(aGoal4._1)(aGoal4._2) == 'A' &&
        burrow(bGoal1._1)(bGoal1._2) == 'B' &&
        burrow(bGoal2._1)(bGoal2._2) == 'B' &&
        burrow(bGoal3._1)(bGoal3._2) == 'B' &&
        burrow(bGoal4._1)(bGoal4._2) == 'B' &&
        burrow(cGoal1._1)(cGoal1._2) == 'C' &&
        burrow(cGoal2._1)(cGoal2._2) == 'C' &&
        burrow(cGoal3._1)(cGoal3._2) == 'C' &&
        burrow(cGoal4._1)(cGoal4._2) == 'C' &&
        burrow(dGoal1._1)(dGoal1._2) == 'D' &&
        burrow(dGoal2._1)(dGoal2._2) == 'D' &&
        burrow(dGoal3._1)(dGoal3._2) == 'D' &&
        burrow(dGoal4._1)(dGoal4._2) == 'D'
    }

    def entranceOccupied(): Option[(Int, Int)] = {
      val positions = amber ++ bronze ++ copper ++ desert
      if (positions.contains(aEntrance)) return Some(aEntrance)
      if (positions.contains(bEntrance)) return Some(bEntrance)
      if (positions.contains(cEntrance)) return Some(cEntrance)
      if (positions.contains(dEntrance)) return Some(dEntrance)
      return None
    }

    def canGetHome(c: Char, pos: (Int, Int)): Boolean = {
      c match {
        case 'A' => {
          //          if (isOccupied(aGoal1) && isOccupied(aGoal2)) return false
          //          if (pos == aGoal2) return false
          //          if (isOccupied(aGoal2) && (pos == aGoal1 || burrow(aGoal2._1)(aGoal2._2) != 'A')) return false
          //          if (pos == aGoal1) return true
          //          if (isOccupied(aGoal1)) return false
          val goals = if (this.first) Array(aGoal1, aGoal2) else Array(aGoal1, aGoal2, aGoal3, aGoal4)
          if (goals.exists(pos => isOccupied(pos) && burrow(pos._1)(pos._2) != 'A')) return false
          var p = pos
          while (p._1 > 1) {
            p = (p._1 - 1, p._2)
            if (isOccupied(p)) return false
          }
          while (p._2 > 3) {
            p = (p._1, p._2 - 1)
            if (isOccupied(p)) return false
          }
          while (p._2 < 3) {
            p = (p._1, p._2 + 1)
            if (isOccupied(p)) return false
          }
          if (isOccupied((p._1 + 1, p._2))) return false
          return true
        }
        case 'B' => {
          //          if (isOccupied(bGoal1) && isOccupied(bGoal2)) return false
          //          if (pos == bGoal2) return false
          //          if (isOccupied(bGoal2) && (pos == bGoal1 || burrow(bGoal2._1)(bGoal2._2) != 'B')) return false
          //          if (pos == bGoal1) return true
          //          if (isOccupied(bGoal1)) return false
          val goals = if (this.first) Array(bGoal1, bGoal2) else Array(bGoal1, bGoal2, bGoal3, bGoal4)
          if (goals.exists(pos => isOccupied(pos) && burrow(pos._1)(pos._2) != 'B')) return false
          var p = pos
          while (p._1 > 1) {
            p = (p._1 - 1, p._2)
            if (isOccupied(p)) return false
          }
          while (p._2 > 5) {
            p = (p._1, p._2 - 1)
            if (isOccupied(p)) return false
          }
          while (p._2 < 5) {
            p = (p._1, p._2 + 1)
            if (isOccupied(p)) return false
          }
          if (isOccupied((p._1 + 1, p._2))) return false
          return true
        }
        case 'C' => {
          //          if (isOccupied(cGoal1) && isOccupied(cGoal2)) return false
          //          if (pos == cGoal2) return false
          //          if (isOccupied(cGoal2) && (pos == cGoal1 || burrow(cGoal2._1)(cGoal2._2) != 'C')) return false
          //          if (pos == cGoal1) return true
          //          if (isOccupied(cGoal1)) return false
          val goals = if (this.first) Array(cGoal1, cGoal2) else Array(cGoal1, cGoal2, cGoal3, cGoal4)
          if (goals.exists(pos => isOccupied(pos) && burrow(pos._1)(pos._2) != 'C')) return false
          var p = pos
          while (p._1 > 1) {
            p = (p._1 - 1, p._2)
            if (isOccupied(p)) return false
          }
          while (p._2 > 7) {
            p = (p._1, p._2 - 1)
            if (isOccupied(p)) return false
          }
          while (p._2 < 7) {
            p = (p._1, p._2 + 1)
            if (isOccupied(p)) return false
          }
          if (isOccupied((p._1 + 1, p._2))) return false
          return true
        }
        case 'D' => {
          //          if (isOccupied(dGoal1) && isOccupied(dGoal2)) return false
          //          if (pos == dGoal2) return false
          //          if (isOccupied(dGoal2) && (pos == dGoal1 || burrow(dGoal2._1)(dGoal2._2) != 'D')) return false
          //          if (pos == dGoal1) return true
          //          if (isOccupied(dGoal1)) return false
          val goals = if (this.first) Array(dGoal1, dGoal2) else Array(dGoal1, dGoal2, dGoal3, dGoal4)
          if (goals.exists(pos => isOccupied(pos) && burrow(pos._1)(pos._2) != 'D')) return false
          var p = pos
          while (p._1 > 1) {
            p = (p._1 - 1, p._2)
            if (isOccupied(p)) return false
          }
          while (p._2 > 9) {
            p = (p._1, p._2 - 1)
            if (isOccupied(p)) return false
          }
          while (p._2 < 9) {
            p = (p._1, p._2 + 1)
            if (isOccupied(p)) return false
          }
          if (isOccupied((p._1 + 1, p._2))) return false
          return true
        }
      }
    }

    def towardsHome(c: Char, pos: (Int, Int)): (Int, Int) = {
      c match {
        case 'A' => {
          if (pos == aGoal1) return aGoal2
          if (pos == aEntrance) return aGoal1
          if (pos._1 > 1) return (pos._1 - 1, pos._2)
          if (pos._2 > 3) return (pos._1, pos._2 - 1)
          return (pos._1, pos._2 + 1)
        }
        case 'B' => {
          if (pos == bGoal1) return bGoal2
          if (pos == bEntrance) return bGoal1
          if (pos._1 > 1) return (pos._1 - 1, pos._2)
          if (pos._2 > 5) return (pos._1, pos._2 - 1)
          return (pos._1, pos._2 + 1)
        }
        case 'C' => {
          if (pos == cGoal1) return cGoal2
          if (pos == cEntrance) return cGoal1
          if (pos._1 > 1) return (pos._1 - 1, pos._2)
          if (pos._2 > 7) return (pos._1, pos._2 - 1)
          return (pos._1, pos._2 + 1)
        }
        case 'D' => {
          if (pos == dGoal1) return dGoal2
          if (pos == dEntrance) return dGoal1
          if (pos._1 > 1) return (pos._1 - 1, pos._2)
          if (pos._2 > 9) return (pos._1, pos._2 - 1)
          return (pos._1, pos._2 + 1)
        }
      }
    }

    def moves(c: Char, a: Vector[(Int, Int)]) = {
      a.zipWithIndex.filter(pos => pos._1._1 != 1 || previousMover == pos._1 || canGetHome(c, pos._1)).map(pos => Array(((pos._1._1 + 1, pos._1._2), pos._2), ((pos._1._1 - 1, pos._1._2), pos._2), ((pos._1._1, pos._1._2 + 1), pos._2), ((pos._1._1, pos._1._2 - 1), pos._2))).flatten.filter(pos => !isOccupied(pos._1))
    }

    def successorMoves(): Array[(Char, Int, (Int, Int))] = {
      if (isDone()) return Array()
      val occupier = entranceOccupied()
      if (occupier.isDefined) {
        val (x, y) = occupier.get
        val c = burrow(x)(y)
        val typeArray = c match {
          case 'A' => amber
          case 'B' => bronze
          case 'C' => copper
          case 'D' => desert
        }
        val i = typeArray.indexOf(occupier.get)
        val positions = Array((x, y - 1), (x, y + 1), (x + 1, y)).filter(!isOccupied(_))
        val finalPositions = c match {
          case 'A' => positions.filter(p => if (((this.first && (burrow(aGoal2._1)(aGoal2._2) == 'A' || burrow(aGoal2._1)(aGoal2._2) == '.')) || (!this.first && (burrow(aGoal2._1)(aGoal2._2) == 'A' || burrow(aGoal2._1)(aGoal2._2) == '.') && (burrow(aGoal3._1)(aGoal3._2) == 'A' || burrow(aGoal3._1)(aGoal3._2) == '.') && (burrow(aGoal4._1)(aGoal4._2) == 'A' || burrow(aGoal4._1)(aGoal4._2) == '.'))) && occupier.get == aEntrance) p == aGoal1 else p != bGoal1 && p != cGoal1 && p != dGoal1)
          case 'B' => positions.filter(p => if (((this.first && (burrow(bGoal2._1)(bGoal2._2) == 'B' || burrow(bGoal2._1)(bGoal2._2) == '.')) || (!this.first && (burrow(bGoal2._1)(bGoal2._2) == 'B' || burrow(bGoal2._1)(bGoal2._2) == '.') && (burrow(bGoal3._1)(bGoal3._1) == 'B' || burrow(bGoal3._1)(bGoal3._2) == '.') && (burrow(bGoal4._1)(bGoal4._2) == 'B' || burrow(bGoal4._1)(bGoal4._2) == '.'))) && occupier.get == bEntrance) p == bGoal1 else p != aGoal1 && p != cGoal1 && p != dGoal1)
          case 'C' => positions.filter(p => if (((this.first && (burrow(cGoal2._1)(cGoal2._2) == 'C' || burrow(cGoal2._1)(cGoal2._2) == '.')) || (!this.first && (burrow(cGoal2._1)(cGoal2._2) == 'C' || burrow(cGoal2._1)(cGoal2._2) == '.') && (burrow(cGoal3._1)(cGoal3._1) == 'C' || burrow(cGoal3._1)(cGoal3._2) == '.') && (burrow(cGoal4._1)(cGoal4._2) == 'C' || burrow(cGoal4._1)(cGoal4._2) == '.'))) && occupier.get == cEntrance) p == cGoal1 else p != bGoal1 && p != aGoal1 && p != dGoal1)
          case 'D' => positions.filter(p => if (((this.first && (burrow(dGoal2._1)(dGoal2._2) == 'D' || burrow(dGoal2._1)(dGoal2._2) == '.')) || (!this.first && (burrow(dGoal2._1)(dGoal2._2) == 'D' || burrow(dGoal2._1)(dGoal2._2) == '.') && (burrow(dGoal3._1)(dGoal3._1) == 'D' || burrow(dGoal3._1)(dGoal3._2) == '.') && (burrow(dGoal4._1)(dGoal4._2) == 'D' || burrow(dGoal4._1)(dGoal4._2) == '.'))) && occupier.get == dEntrance) p == dGoal1 else p != bGoal1 && p != aGoal1 && p != cGoal1)
        }
        return finalPositions.map((c, i, _))
      }

      //if (amber.exists( canGetHome('A', _) )) return amber.zipWithIndex.filter( pos => canGetHome('A',pos._1) ).map( pos => ('A', pos._2, towardsHome('A',pos._1)) ).toArray
      //if (bronze.exists( canGetHome('B', _) )) return bronze.zipWithIndex.filter( pos => canGetHome('B',pos._1) ).map( pos => ('B', pos._2, towardsHome('B',pos._1)) ).toArray
      //if (copper.exists( canGetHome('C', _) )) return copper.zipWithIndex.filter( pos => canGetHome('C',pos._1) ).map( pos => ('C', pos._2, towardsHome('C',pos._1)) ).toArray
      //if (desert.exists( canGetHome('D', _) )) return desert.zipWithIndex.filter( pos => canGetHome('D',pos._1) ).map( pos => ('D', pos._2, towardsHome('D',pos._1)) ).toArray

      val res = ArrayBuffer[(Char, Int, (Int, Int))]()
      res ++= moves('A', amber).map(pos => ('A', pos._2, pos._1))
      res ++= moves('B', bronze).map(pos => ('B', pos._2, pos._1))
      res ++= moves('C', copper).map(pos => ('C', pos._2, pos._1))
      res ++= moves('D', desert).map(pos => ('D', pos._2, pos._1))
      return res.toArray
    }

    def successors(): Array[((Char, Int, (Int, Int)), State)] = {
      val moves = this.successorMoves()
      return moves.map(m => (m, this.move(m._1, m._2, m._3)))
    }

    def key1() = (amber(0), amber(1), bronze(0), bronze(1), copper(0), copper(1), desert(0), desert(1))
    def key2() = {
      (amber(0), amber(1), amber(2), amber(3), bronze(0), bronze(1), bronze(2), bronze(3), copper(0), copper(1), copper(2), copper(3), desert(0), desert(1), desert(2), desert(3))
    }

    def manhattan(pos1: (Int, Int), pos2: (Int, Int)) = abs(pos2._1 - pos1._1) + abs(pos2._2 - pos1._2)

    def amberHeuristic(pos: (Int, Int)) = {
      if (pos == aGoal1 && burrow(aGoal2._1)(aGoal2._2) == '.') 1L else if (pos == aGoal1 && burrow(aGoal2._1)(aGoal2._2) != 'A') 4L else if (pos == aGoal2 || pos == aGoal1) 0L else abs(pos._1 - 1).toLong + manhattan((1, pos._2), aGoal1).toLong
    }
    def bronzeHeuristic(pos: (Int, Int)) = {
      if (pos == bGoal1 && burrow(bGoal2._1)(bGoal2._2) == '.') 1L else if (pos == bGoal1 && burrow(bGoal2._1)(bGoal2._2) != 'B') 4L else if (pos == bGoal2 || pos == bGoal1) 0L else abs(pos._1 - 1).toLong + manhattan((1, pos._2), bGoal1).toLong
    }
    def copperHeuristic(pos: (Int, Int)) = {
      if (pos == cGoal1 && burrow(cGoal2._1)(cGoal2._2) == '.') 1L else if (pos == cGoal1 && burrow(cGoal2._1)(cGoal2._2) != 'C') 4L else if (pos == cGoal2 || pos == cGoal1) 0L else abs(pos._1 - 1).toLong + manhattan((1, pos._2), cGoal1).toLong
    }
    def desertHeuristic(pos: (Int, Int)) = {
      if (pos == dGoal1 && burrow(dGoal2._1)(dGoal2._2) == '.') 1L else if (pos == dGoal1 && burrow(dGoal2._1)(dGoal2._2) != 'D') 4L else if (pos == dGoal2 || pos == dGoal1) 0L else abs(pos._1 - 1).toLong + manhattan((1, pos._2), dGoal1).toLong
    }

    def heuristic(): Long = {
      if (isDone()) return 0L
      val a = amber.map(amberHeuristic(_))
      val b = bronze.map(bronzeHeuristic(_))
      val c = copper.map(copperHeuristic(_))
      val d = desert.map(desertHeuristic(_))
      return a.sum + b.sum * 10L + c.sum * 100L + d.sum * 1000L
    }

    override def toString() = {
      var res = ""
      for (line <- this.burrow) {
        for (char <- line) {
          res += char
        }
        res += "\n"
      }
      res
    }
  }

  class State2(val burrow: Array[Array[Char]]) {
    def isDone(): Boolean = {
      for (a <- this.amphipods) {
        //println(a._2)
        if (a._1._2 != destY(a._2)) {
          return false
        }
      }
      return true
    }

    def manhattan(pos1: (Int, Int), pos2: (Int, Int)) = abs(pos2._1 - pos1._1) + abs(pos2._2 - pos1._2)

    def move2(m: (Int, Int, Int)): (State2, Int) = {
      val ((x, y), c) = this.amphipods(m._1)
      val resBurrow = burrow.clone().map(_.clone())
      resBurrow(x)(y) = '.'
      resBurrow(m._2)(m._3) = c
      val cost = manhattan((x, y), (m._2, m._3)) * (c match { case 'A' => 1 case 'B' => 10 case 'C' => 100 case 'D' => 1000 })
      (new State2(resBurrow), cost)
    }

    val amphipods = for (x <- 0 until burrow.size; y <- 0 until burrow(0).size if burrow(x)(y) != '#' && burrow(x)(y) != '.') yield ((x, y), burrow(x)(y)) //burrow.map( pos => (pos, burrow(pos._1)(pos._2)) )

    val destY = Map[Char, Int]('A' -> 3, 'B' -> 5, 'C' -> 7, 'D' -> 9)

    def hallwayClear(start: Int, stop: Int, ind: Int): Boolean = {
      val (x1, x2) = (start min stop, start max stop)
      for ((a, i) <- this.amphipods.zipWithIndex) {
        if (i != ind && a._1._1 == 1 && a._1._2 >= x1 && a._1._2 <= x2) {
          return false
        }
      }
      return true
    }

    val hallwayCoords = Array(1, 2, 4, 6, 8, 10, 11)

    def successors2(): Array[(State2, Int)] = {
      val moves = ListBuffer[(Int, Int, Int)]()

      for ((a, i) <- this.amphipods.zipWithIndex) {
        val dy = destY(a._2)
        if (a._1._1 != 5 || a._1._2 != dy) {
          if (a._1._1 == 1) {
            var dx = -1
            if ((2 to 5).forall(x => burrow(x)(dy) == a._2 || burrow(x)(dy) == '.')) {
              dx = (5 to 2 by -1).find(x => burrow(x)(dy) == '.').get
            }

            if (dx != -1 && this.hallwayClear(a._1._2, dy, i)) {
              moves += ((i, dx, dy))
            }
          } else {
            var isFreeToMove = true
            var x = 2
            while (x < a._1._1) {
              if (burrow(x)(a._1._2) != '.') {
                isFreeToMove = false
                x = a._1._1
              }
              x += 1
            }
            if (isFreeToMove) {
              var isDone = false
              if (a._1._2 == dy) {
                isDone = true
                var x = a._1._1 + 1
                while (x <= 5) {
                  if (burrow(x)(a._1._2) != a._2) {
                    isDone = false
                    x = 6
                  }
                  x += 1
                }
              }
              if (!isDone) {
                for (y <- this.hallwayCoords) {
                  if (this.hallwayClear(a._1._2, y, i)) {
                    moves += ((i, 1, y))
                  }
                }
              }
            }
          }
        }
      }

      return moves.map(m => this.move2(m)).toArray
    }

    def encodeChar(s: Char): Int = {
      s match {
        case '.' => 0
        case 'A' => 1
        case 'B' => 2
        case 'C' => 3
        case 'D' => 4
      }
    }

    def encode(): Long = {
      val vals = new Array[Int](27)
      for ((pos, i) <- positions.zipWithIndex) {
        vals(i) = encodeChar(burrow(pos._1)(pos._2))
      }

      var result = 0L
      for (v <- vals) {
        result *= 5
        result += v
      }
      return result
    }
    override def toString() = {
      var res = ""
      for (line <- this.burrow) {
        for (char <- line) {
          res += char
        }
        res += "\n"
      }
      res
    }
  }

  def aStar(startingState: State): Long = {
    val q = PriorityQueue[(Long, State)]()(Ordering.by(-_._1))
    val pred1 = HashMap[((Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int)), ((Char, Int, (Int, Int)), State)]()
    val pred2 = HashMap[((Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int)), ((Char, Int, (Int, Int)), State)]()
    val min1 = HashMap[((Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int)), Long]()
    val min2 = HashMap[((Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int), (Int, Int)), Long]()
    def m(s: State): Long = {
      if (s.first) {
        if (min1.contains(s.key1())) min1(s.key1())
        else Long.MaxValue
      } else if (min2.contains(s.key2())) min2(s.key2())
      else Long.MaxValue
    }

    var best = Long.MaxValue
    var goalState = startingState

    if (startingState.first) min1(startingState.key1()) = 0L
    else min2(startingState.key2()) = 0L
    q.enqueue((startingState.heuristic(), startingState))
    var terminate = false
    while (q.nonEmpty && !terminate) {
      val (f, state) = q.dequeue()
      if (best < f) terminate = true
      else {
        for ((move, succ) <- state.successors) {
          //print(succ)
          //println(move)
          val newCost = m(state) + (move._1 match {
            case 'A' => 1L
            case 'B' => 10L
            case 'C' => 100L
            case 'D' => 1000L
          })
          if (newCost < m(succ)) {
            if (succ.first) min1(succ.key1()) = newCost
            else min2(succ.key2()) = newCost
            if (succ.first) pred1(succ.key1()) = (move, state)
            else pred2(succ.key2()) = (move, state)
            if (succ.isDone()) {
              val newBest = if (succ.first) min1(succ.key1()) else min2(succ.key2())
              if (newBest < best) {
                best = newBest
                goalState = succ
              }
            } else {
              q.enqueue((m(succ) + succ.heuristic(), succ))
            }
          }
        }
      }
    }
    if ((startingState.first && goalState.key1() == startingState.key1()) || (!startingState.first && goalState.key2() == startingState.key2())) println("Not working")
    best
  }

  //val path = ListBuffer[(Char, Int, (Int,Int))]()
  //var p = pred(goalState.key())
  //while (p._2.key() != startingState.key()) {
  //  path.prepend(p._1)
  //  p = pred(p._2.key())
  //}
  //path.prepend(p._1)

  //var s = startingState
  //println(s)
  //path.foreach( m => {s = s.move(m._1, m._2, m._3); println(s)} )
  //println(path.map(_._1 match {case 'A' => 1 case 'B' => 10 case 'C' => 100 case 'D' => 1000}).sum)

  val startingState = new State(origBurrow, origAmber.toVector, origBronze.toVector, origCopper.toVector, origDesert.toVector, (0, 0), true)
  val best = aStar(startingState)
  println("Least energy required to organize the amphipods: " + best)

  val origBurrow2 = Array(
    Array('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'),
    Array('#', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '.', '#'),
    Array('#', '#', '#', 'D', '#', 'B', '#', 'C', '#', 'C', '#', '#', '#'),
    Array('#', '#', '#', 'D', '#', 'C', '#', 'B', '#', 'A', '#', '#', '#'),
    Array('#', '#', '#', 'D', '#', 'B', '#', 'A', '#', 'C', '#', '#', '#'),
    Array('#', '#', '#', 'D', '#', 'A', '#', 'B', '#', 'A', '#', '#', '#'),
    Array('#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#', '#'))

  //  val origAmber2 = Array((3, 9), (4, 7), (5, 5), (5, 9))
  //  val origBronze2 = Array((2, 5), (4, 5), (3, 7), (5, 7))
  //  val origCopper2 = Array((2, 7), (2, 9), (3, 5), (4, 9))
  //  val origDesert2 = Array((2, 3), (3, 3), (4, 3), (5, 3))

  def dijkstra(source: State2): Int = {
    //var printed = 0
    val distt = HashMap[Long, Int]()
    val predd = HashMap[Long, Long]()
    def dist(s: State2) = {
      val e = s.encode()
      if (distt.contains(e)) distt(e)
      else Int.MaxValue
    }
    def pred(s: State2) = {
      val e = s.encode()
      if (predd.contains(e)) predd(e)
      else null
    }
    distt(source.encode()) = 0
    val res = PriorityQueue[(Int, Long)]()(Ordering.by(-_._1))
    val pq = PriorityQueue[(Int, Long)]()(Ordering.by(-_._1))
    pq.enqueue((0, source.encode()))
    //var i = 0L
    while (pq.nonEmpty && (res.isEmpty || pq.head._1 < res.head._1)) {
      val (d, u) = pq.dequeue()
      val uu = decode(u)
      if (uu.isDone()) {
        res.enqueue((d, u))
        //println("Found " + i)
      }
      for ((v, cost) <- uu.successors2()) {
        if (dist(v) == Int.MaxValue || d + cost < dist(v)) {
          val e = v.encode()
//          if (printed < 10) {
//            println(v)
//            printed += 1
//          }
          distt(e) = d + cost
          predd(e) = u
          pq.enqueue((dist(v), e))
        }
      }
      //i += 1
    }
    res.dequeue()._1
  }

  def decodePos(p: Int): (Int, Int) = {
    return positions(p)
  }

  val kinds = Array('A', 'B', 'C', 'D')

  def decodeKind(x: Int): Char = {
    return kinds(x - 1)
  }

  def decode(nn: Long): State2 = {
    var i = 0
    var n = nn
    var pos = 26
    val resBurrow = origBurrow2.map(_.map(c => if (c == '#') c else '.'))
    while (n > 0) {
      val v = n % 5
      if (v > 0) {
        val (x, y) = decodePos(pos)
        val k = decodeKind(v.toInt)
        //println(k)
        resBurrow(x)(y) = k
        i += 1
      }
      n = n / 5
      pos -= 1
    }
    //	  val resAmber = (for (pos <- positions if resBurrow(pos._1)(pos._2) == 'A') yield (pos._1,pos._2)).toVector
    //	  val resBronze = (for (pos <- positions if resBurrow(pos._1)(pos._2) == 'B') yield (pos._1,pos._2)).toVector
    //	  val resCopper = (for (pos <- positions if resBurrow(pos._1)(pos._2) == 'C') yield (pos._1,pos._2)).toVector
    //	  val resDesert = (for (pos <- positions if resBurrow(pos._1)(pos._2) == 'D') yield (pos._1,pos._2)).toVector
    new State2(resBurrow)
  }

  val startingState2 = new State2(origBurrow2) //, origAmber2.toVector, origBronze2.toVector, origCopper2.toVector, origDesert2.toVector, (0,0), false)
  //val a = startingState2.encode()
  //val s = decode(a)
  //println(s)
  val best2 = dijkstra(startingState2)
  println("Least energy required to organize the amphipods the second time: " + best2)
}