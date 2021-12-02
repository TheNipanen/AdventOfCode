

package day13

object ShuttleSearch extends App {
  val earliest = 1004098L
  val IDs = "23,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,509,x,x,x,x,x,x,x,x,x,x,x,x,13,17,x,x,x,x,x,x,x,x,x,x,x,x,x,x,29,x,401,x,x,x,x,x,37,x,x,x,x,x,x,x,x,x,x,x,x,19".split(",").zipWithIndex.filter( _._1 != "x" ).map( id => (id._1.toInt, id._2) )
  
  var found = false
  var time = earliest
  var res = -1
  var resTime = -1L
  while (!found) {
    for (ID <- IDs) {
      if (time % ID._1 == 0) {
        found = true
        res = ID._1
        resTime = time
      }
    }
    time += 1
  }
  val waitTime = resTime - earliest
  println("ID: " + res)
  println("Waiting time: " + (waitTime))
  println("Multiplied: " + (res * waitTime))
  
  val IDs2 = IDs.sortBy(-_._1).map( { case (n,a) => {
    var ai = -a
    while (ai < 0) {
      ai = n + ai
    }
    ai = ai % n
    (n, ai)
  }} )
  //IDs.foreach(println(_))
  //println()
  //IDs2.foreach(println(_))
  resTime = IDs2(0)._2
  var additive = IDs2(0)._1.toLong
  var i = 1
  while (i < IDs2.length) {
    val divisor = IDs2(i)._1.toLong
    val res = IDs2(i)._2.toLong
    //println(resTime)
    while (resTime % divisor != res) {
      //println(resTime)
      resTime += additive
    }
    additive *= divisor
    i += 1
  }
  
  println("Earliest match: " + resTime)
}