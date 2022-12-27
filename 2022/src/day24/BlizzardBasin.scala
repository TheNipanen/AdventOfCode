package day24

import scala.io.Source
import scala.collection.mutable.{ HashMap, Queue }

class State(val pos: (Int, Int), val blizz: HashMap[(Int, Int), (Int, Int)]) {
  override def hashCode() = {
    blizz.map( _._1.hashCode() ).sum + pos.hashCode()
  }
  
  override def equals(other: Any) = {
    other.isInstanceOf[State] && other.asInstanceOf[State].pos == this.pos && other.asInstanceOf[State].blizz.map( _._1 ).sameElements(this.blizz.map( _._1 ))
  }
}

object BlizzardBasin extends App {
  val s = Source.fromFile("src/day24/input.txt")
  val grid = try s.getLines().toArray.map( _.toCharArray() ) finally s.close()
  
  val blizzards = HashMap[(Int, Int), (Int, Int)]()
  val dist = HashMap[State, Int]()
  
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
        blizzards((i, j)) = dir
      }
      j += 1
    }
    i += 1
  }
  
  def bfs(): Int = {
    val source = new State((0, 1), blizzards)
    dist(source) = 0
    val q = Queue[State]()
    q.enqueue(source)
    while (q.nonEmpty) {
      val u = q.dequeue()
      
    }
  }
}