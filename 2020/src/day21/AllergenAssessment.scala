package day21

import scala.io.Source

object AllergenAssessment extends App {
  val s = Source.fromFile("src/day21/input.txt")
  val r = s.bufferedReader()
  var line = r.readLine()
  
  var ingredients = Array[String]()
  val allergenes = scala.collection.mutable.HashMap[String, Set[String]]()
  
  while (line != null) {
    val (i, a) = line.splitAt(line.indexOf('('))
    val ingr = i.split(" ").filter( _ != "" ).toSet
    ingredients = ingredients ++ ingr
    val allerg = a.filter( c => c != ' ' && c != '(' && c != ')' ).drop(8).split(",")
    for (allergene <- allerg) {
      if (allergenes.contains(allergene)) allergenes(allergene) = allergenes(allergene).intersect(ingr)
      else allergenes(allergene) = ingr
    }
    line = r.readLine()
  }
  val L = allergenes.size
  
  var count = 0
  val containAllergenes = allergenes.values.foldLeft(Set[String]())( _ union _ )
  val safe = scala.collection.mutable.ArrayBuffer[String]()
  for (ingredient <- ingredients) {
    if (!containAllergenes.contains(ingredient)) {
      count += 1
      safe += ingredient
    }
  }
  println("Safe ingredients: " + count)
  
  for ((a, i) <- allergenes) {
    allergenes(a) = i -- safe
  }
  //println(allergenes)
  
  val res = scala.collection.mutable.HashMap[String, String]()
  var i = 0
  while (i < L) {
    val all = allergenes.toArray.sortBy( _._2.size )
    val ones = all.takeWhile( _._2.size == 1 )
    ones.foreach( {case (a, i) => res(a) = i.head} )
    for ((a, i) <- allergenes) {
      allergenes(a) = i -- res.values
      if (allergenes(a).size == 0) allergenes.remove(a)
    }
    i += 1
  }
  assert(res.size == L)
  val result = res.toArray.sortBy( _._1 ).map( _._2 ).mkString(",")
  println("Canonical dangerous ingredient list: " + result)
}