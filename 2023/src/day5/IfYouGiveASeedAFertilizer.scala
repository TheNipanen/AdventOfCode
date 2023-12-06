package day5

import scala.io.Source
import scala.collection.mutable.{ ArrayBuffer, HashMap, ListBuffer }

object IfYouGiveASeedAFertilizer extends App {
  val s = Source.fromFile("src/day5/input.txt")
  val lines = try s.getLines().toArray finally s.close()

  val seeds = lines(0).drop(6).trim().split(" ").map(_.toLong)
  val seedToSoil = HashMap[(Long, Long), (Long, Long)]()
  val soilToFertilizer = HashMap[(Long, Long), (Long, Long)]()
  val fertilizerToWater = HashMap[(Long, Long), (Long, Long)]()
  val waterToLight = HashMap[(Long, Long), (Long, Long)]()
  val lightToTemperature = HashMap[(Long, Long), (Long, Long)]()
  val temperatureToHumidity = HashMap[(Long, Long), (Long, Long)]()
  val humidityToLocation = HashMap[(Long, Long), (Long, Long)]()

  def process(ds: Long, ss: Long, length: Long, map: HashMap[(Long, Long), (Long, Long)]): Unit = {
    var sourceStart = ss
    var destStart = ds
    var sourceEnd = sourceStart + length - 1
    var destEnd = destStart + length - 1
    map((sourceStart, sourceEnd)) = (destStart, destEnd)
  }

  val wordToMap = HashMap[String, HashMap[(Long, Long), (Long, Long)]](
    "seed" -> seedToSoil,
    "soil" -> soilToFertilizer,
    "fertilizer" -> fertilizerToWater,
    "water" -> waterToLight,
    "light" -> lightToTemperature,
    "temperature" -> temperatureToHumidity,
    "humidity" -> humidityToLocation)

  var i = 2
  val l = lines.length
  while (i < l) {
    var line = lines(i)
    val map = wordToMap(line.takeWhile(_ != '-'))
    i += 1
    while (i < l && lines(i).length > 0) {
      line = lines(i)
      val numbers = line.split(" ").map(_.toLong)
      process(numbers(0), numbers(1), numbers(2), map)
      i += 1
    }
    i += 1
  }

  def getDest(source: Long, map: HashMap[(Long, Long), (Long, Long)]): Long = {
    for ((start, end) <- map.keys) {
      if (source >= start && source <= end) {
        val (destStart, destEnd) = map((start, end))
        val diff = source - start
        return destStart + diff
      }
    }
    source
  }

  var minLocation = Long.MaxValue
  
  def seedToLocation(seed: Long): Long = {
    val soil = getDest(seed, seedToSoil)
    val fertilizer = getDest(soil, soilToFertilizer)
    val water = getDest(fertilizer, fertilizerToWater)
    val light = getDest(water, waterToLight)
    val temperature = getDest(light, lightToTemperature)
    val humidity = getDest(temperature, temperatureToHumidity)
    getDest(humidity, humidityToLocation)
  }
  
  for (seed <- seeds) {
    val location = seedToLocation(seed)
    minLocation = minLocation min location
  }

  println("Lowest location number: " + minLocation)

  def overlaps(r1: (Long, Long), r2: (Long, Long)): Boolean = {
    r1._1 <= r2._2 && r2._1 <= r1._2
  }

  def getDestRanges(range: (Long, Long), map: HashMap[(Long, Long), (Long, Long)]): ListBuffer[(Long, Long)] = {
    val result = ListBuffer[(Long, Long)]()
    val overlapping = ArrayBuffer[(Long, Long)]()
    overlapping ++= map.keys.filter(overlaps(range, _)).toArray.sortBy( _._1 )
    var i = range._1
    while (i <= range._2) {
      val olap = overlapping.find( r => i >= r._1 && i <= r._2 )
      if (olap.isDefined) {
        val (olS, olE) = olap.get
        val diffEnd = (olE - range._2) max 0L
        val diffStart = i - olS
        val (ds, de) = map((olS, olE))
        result += ((ds + diffStart, de - diffEnd))
        i = olE + 1L
        overlapping -= ((olS, olE))
      } else {
        if (overlapping.length == 0) {
          result += ((i, range._2))
          i = range._2 + 1
        } else {
          val next = overlapping(0)
          result += ((i, next._1 - 1))
          i = next._1
        }
      }
    }

    result
  }

  def combineRanges(ranges: Array[(Long, Long)]): Array[(Long, Long)] = {
    val r = ranges.toBuffer
    for (c <- ranges) {
      if (r.contains(c)) {
        var current = c
        var overlap = r.find(overlaps(current, _))
        while (overlap.isDefined) {
          val ol = overlap.get
          val start = current._1 min ol._1
          val end = current._2 max ol._2
          current = (start, end)
          r -= ol
          overlap = r.find(overlaps(current, _))
        }
        r += current
      }
    }
    r.toArray
  }

  def getDestRanges(ranges: Array[(Long, Long)], map: HashMap[(Long, Long), (Long, Long)]): Array[(Long, Long)] = {
    val result = ListBuffer[(Long, Long)]()
    result ++= ranges.flatMap(getDestRanges(_, map))

    combineRanges(result.toArray)
    //result.toArray
  }

  minLocation = Long.MaxValue
  val seedsRanges = seeds.grouped(2).map(arr => (arr(0), arr(0) + arr(1))).toArray
  val soils = getDestRanges(seedsRanges, seedToSoil)
  val fertilizers = getDestRanges(soils, soilToFertilizer)
  val waters = getDestRanges(fertilizers, fertilizerToWater)
  val lights = getDestRanges(waters, waterToLight)
  val temperatures = getDestRanges(lights, lightToTemperature)
  val humidities = getDestRanges(temperatures, temperatureToHumidity)
  val locations = getDestRanges(humidities, humidityToLocation).map(_._1)
  minLocation = minLocation min locations.min

  println("Lowest location number with ranges: " + minLocation)
}