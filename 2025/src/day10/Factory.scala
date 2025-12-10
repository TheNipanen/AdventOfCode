package day10
import scala.io.Source
import scala.collection.mutable.{HashSet, Queue}
import com.microsoft.z3.{ Context, IntExpr, IntNum, Status }

object Factory extends App {
  val s = Source.fromFile("src/day10/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  def click(lights: String, button: Array[Int]) = {
    var result = lights
    for (i <- button) {
      val toggle = if (result(i) == '#') '.' else '#'
      result = result.updated(i, toggle)
    }
    result
  }
  
  var totalPresses = 0
  var totalPresses2 = 0
  
  for (line <- lines) {
    val lights = line.drop(1).takeWhile( _ != ']' )
    val buttons = line.dropWhile( _ != '(' ).takeWhile( _ != '{' ).dropRight(1).split(" ").map( _.drop(1).dropRight(1).split(",").map( _.toInt ) )
    
    val q = Queue[(String, Int)]()
    var found = -1
    q.enqueue(("." * lights.size, 0))
    val visited = HashSet[String]()
    while (found == -1 && q.nonEmpty) {
      val u = q.dequeue()
      visited(u._1) = true
      if (u._1 == lights) {
        found = u._2
      } else {
        val neighbors = buttons.map( click(u._1, _) )
        for (neighbor <- neighbors) {
          if (!visited(neighbor)) {
            q.enqueue((neighbor, u._2 + 1))
          }
        }
      }
    }
    totalPresses += found
    
    val counters = line.dropWhile( _ != '{' ).drop(1).dropRight(1).split(",").map( _.toInt )
    val maxCounterTarget = counters.max
    
    //val model = new CpModel()
    val ctx = new Context()
    val opt = ctx.mkOptimize()
    val buttonPressVariables = buttons.zipWithIndex.map( pair => ctx.mkIntConst("button" + pair._2) )
    
    var i = 0
    while (i < counters.size) {
      val buttonPressVariablesAffectingCounter = buttons.zipWithIndex.filter( _._1.contains(i) ).map( pair => buttonPressVariables(pair._2) )
      var sumOfAffectingButtonPresses = buttonPressVariablesAffectingCounter(0)
      var j = 1
      while (j < buttonPressVariablesAffectingCounter.size) {
        sumOfAffectingButtonPresses = ctx.mkAdd(sumOfAffectingButtonPresses, buttonPressVariablesAffectingCounter(j)).asInstanceOf[IntExpr]
        j += 1
      }
      val target = ctx.mkInt(counters(i))
      val equality = ctx.mkEq(sumOfAffectingButtonPresses, target)
      opt.Add(equality)
      
      i += 1
    }
    
    val zero = ctx.mkInt(0)
    for (variable <- buttonPressVariables) {
      val nonNegative = ctx.mkGe(variable, zero)
      opt.Add(nonNegative)
    }
    
    var sumOfButtonPressVariables = buttonPressVariables(0)
    i = 1
    while (i < buttonPressVariables.size) {
      sumOfButtonPressVariables = ctx.mkAdd(sumOfButtonPressVariables, buttonPressVariables(i)).asInstanceOf[IntExpr]
      i += 1
    }
    val buttonPresses = ctx.mkIntConst("buttonPresses")
    val equality = ctx.mkEq(buttonPresses, sumOfButtonPressVariables)
    opt.Add(equality)
    
    opt.MkMinimize(buttonPresses)
    
    val status = opt.Check()
    
    if (status != Status.SATISFIABLE) {
      println("STATUS NOT SOLVED", status)
    } else {
      val model = opt.getModel()
      val presses = model.evaluate(buttonPresses, false).asInstanceOf[IntNum].getInt()
      totalPresses2 += presses
    }
  }
  
  println(totalPresses)
  println(totalPresses2)
}