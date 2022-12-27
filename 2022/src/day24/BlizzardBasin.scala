package day24

import scala.io.Source
import scala.collection.mutable.{ ArrayBuffer, HashMap, PriorityQueue }

class State(val pos: (Int, Int), val blizz: Int) {
  override def hashCode() = {
    blizz.hashCode() + pos.hashCode()
  }
  
  def canEqual(that: Any) = that.isInstanceOf[State]
  
  override def equals(that: Any) = {
    this.canEqual(that) && that.asInstanceOf[State].pos == this.pos && that.asInstanceOf[State].blizz == this.blizz
  }
}

object BlizzardBasin extends App {
  val s = Source.fromFile("src/day24/input.txt")
  val grid = try s.getLines().toArray.map( _.toCharArray() ) finally s.close()
  
  val blizzards = ArrayBuffer[((Int, Int), (Int, Int))]()
  
  var i = 0
  while (i < grid.length) {
    val row = grid(i)
    var j = 0
    while (j < row.length) {
      val c = row(j)
      if (c != '.' && c != '#') {
        row(j) = '.'
        val dir = c match {
          case '>' => (0, 1)
          case '<' => (0, -1)
          case '^' => (-1, 0)
          case 'v' => (1, 0)
        }
        blizzards += (((i, j), dir))
      }
      j += 1
    }
    i += 1
  }
  
  // Simulate the blizzards until a cycle is detected
  var continue = true
  val phases = ArrayBuffer[ArrayBuffer[((Int, Int), (Int, Int))]](blizzards)
  while (continue) {
    val next = nextBlizz(phases(phases.length - 1))
    if (next.sameElements(phases(0))) {
      continue = false
    } else {
      phases += next
    }
  }
  //println("Simulation done")
  
  def isFree(pos: (Int, Int)) = {
    pos._1 >= 0 && pos._1 < grid.length && pos._2 >= 0 && pos._2 < grid(0).length && grid(pos._1)(pos._2) != '#'
  }
  def nextBlizz(blizz: ArrayBuffer[((Int, Int), (Int, Int))]): ArrayBuffer[((Int, Int), (Int, Int))] = {
    val next = ArrayBuffer[((Int, Int), (Int, Int))]()
    blizz.foreach( pair => {
      var nextY = pair._1._1 + pair._2._1
      var nextX = pair._1._2 + pair._2._2
      if (!isFree((nextY, nextX))) {
        while (isFree((nextY - pair._2._1, nextX - pair._2._2))) {
          nextY -= pair._2._1
          nextX -= pair._2._2
        }
      }
      next += (((nextY, nextX), pair._2))
    })
    next
  }
  def neighbors(pos: (Int, Int)) = {
    Array((pos._1 - 1, pos._2), (pos._1 + 1, pos._2), (pos._1, pos._2 - 1), (pos._1, pos._2 + 1), pos).filter( isFree(_) )
  }
  
  def aStar(startPos: (Int, Int), goalPos: (Int, Int), blizzInd: Int): (Int, Int) = {
    val q = PriorityQueue[(Int, State)]()(Ordering.by(-_._1)) // Min priority queue with the priority value as key (cost so far + lower bound for remaining cost)
    val min = HashMap[State, Int]() // HashMap containing the known minimum distance from the start node for each node
    val cost = 1 // The cost of moving from one state to another. It is always one.

    def m(s: State): Int = {
      if (min.contains(s)) min(s)
      else Int.MaxValue
    }
    var best = Int.MaxValue // The cost of the optimal path
    val source = new State(startPos, blizzInd)
    var goalState = source // The eventual goal state

    /** Returns the manhattan distance between two points. Used by the heuristic function. */
    def distance(cPos: (Int, Int), gPos: (Int, Int)) = {
      val dX = math.abs(cPos._1 - gPos._1)
      val dY = math.abs(cPos._2 - gPos._2)
      dX + dY
    }
    
    min(source) = 0
    q.enqueue((distance(source.pos, goalPos), source))
    var terminate = false
    while (q.nonEmpty && !terminate) {
      val (f, state) = q.dequeue()
      if (best < f) terminate = true
      else {
        val nextI = (state.blizz + 1) % phases.length
        val nextBlizzards = phases(nextI)
        val neighborPos = neighbors(state.pos)
        for (n <- neighborPos.filter(  n => !nextBlizzards.exists( _._1 == n ) )) {
          val succ = new State(n, nextI)
          val newCost = m(state) + cost
          if (newCost < m(succ)) {
            min(succ) = newCost
            if (succ.pos == goalPos) {
              val newBest = min(succ)
              if (newBest < best) {
                best = newBest
                goalState = succ
              }
            } else {
              q.enqueue((min(succ) + distance(succ.pos, goalPos), succ))
            }
          }
        }
      }
    }
    if (goalState == source) {
      println("Error, goal not reachable")
      return (-1, blizzInd)
    }

    return (best, goalState.blizz)
  }
  
  val start = (0, 1)
  val end = (grid.length - 1, grid(grid.length - 1).length - 2)
  val minutes = aStar(start, end, 0)
  println("Fewest number of minutes to reach the goal: " + minutes._1)
  val back = aStar(end, start, minutes._2)
  val goal = aStar(start, end, back._2)
  println("Fewest number of minutes to reach goal, go back, and reach goal again: " + (minutes._1 + back._1 + goal._1)) 
}