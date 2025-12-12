package day12
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object ChristmasTreeFarm extends App {
  val s = Source.fromFile("src/day12/input.txt")
  val lines = try s.getLines().toArray finally s.close()
  
  val presents = ArrayBuffer[Array[String]]()
  var i = 0
  while (!lines(i).contains('x')) {
    val present = lines.drop(i).drop(1).take(3)
    presents += present
    i += 5
  }
  
  val presentAreas = presents.toArray.map( _.map( _.count( _ == '#' ) ).sum )
  
  var cannot = 0
  var unknown = 0
  
  while (i < lines.size) {
    val line = lines(i)
    val width = line.takeWhile( _ != 'x' ).toInt
    val height = line.dropWhile( _ != 'x' ).drop(1).takeWhile( _ != ':' ).toInt
    val neededPresents = line.dropWhile( _ != ' ').drop(1).split(" ").map( _.toInt )
    val area = width * height
    val areaNeeded = neededPresents.zipWithIndex.map( pair => pair._1 * presentAreas(pair._2) ).sum
    if (areaNeeded > area) cannot += 1
    else unknown += 1
    i += 1
  }
  // It seems all the unknown ones can fit the presents
  println(cannot, unknown)
}