package day20

import scala.io.Source

object JurassicJigsaw extends App {
  val s = Source.fromFile("src/day20/input.txt")
  val r = s.bufferedReader()
  val images = scala.collection.mutable.HashMap[Long, Array[Array[Char]]]()
  var line = ""
  while (line != null) {
    line = r.readLine()
    val id = line.drop(5).takeWhile( _.isDigit ).toLong
    line = r.readLine()
    val image = scala.collection.mutable.ArrayBuffer[Array[Char]]()
    while (line != null && line != "") {
      val i = line.toCharArray()
      image += i
      line = r.readLine()
    }
    images(id) = image.toArray
  }
  val gL = math.sqrt(images.size)
  //println(gL)
  assert(gL == gL.toInt)
  val gridL = gL.toInt
  //println(gridL)
  val l = images(2927).length // Just some image, they all share the same length
  val result = new Array[Array[Array[Char]]](images.size)
  val resultID = Array.fill[Long](images.size)(-1L)
//  print(images(2927))
//  println()
//  print(rotate(images(2927)))
  
  def print(image: Array[Array[Char]]): Unit = {
    for (row <- image) {
      val str = row.foldLeft("")( _ + _ )
      println(str)
    }
  }
  def rotate(image: Array[Array[Char]]): Array[Array[Char]] = {
    image.transpose.map( _.reverse )
  }
  def flipTopBot(image: Array[Array[Char]]): Array[Array[Char]] = {
    image.reverse
  }
//  def flipLeftRight(image: Array[Array[Char]]): Array[Array[Char]] = {
//    image.map( _.reverse )
//  }
  def orientations(image: Array[Array[Char]]): Array[Array[Array[Char]]] = {
    val res = scala.collection.mutable.ArrayBuffer[Array[Array[Char]]]()
    var current = image
    var i = 0
    while (i < 4) {
      res += current
      res += flipTopBot(current)
      //res += flipLeftRight(current)
      current = rotate(current)
      i += 1
    }
    res.toArray
  }
  def getRightCol(image: Array[Array[Char]]): Array[Char] = {
    image.map( _.last )
  }
  def getLeftCol(image: Array[Array[Char]]): Array[Char] = {
    image.map( _.head )
  }
  def getTopRow(image: Array[Array[Char]]): Array[Char] = {
    image.head
  }
  def getBotRow(image: Array[Array[Char]]): Array[Char] = {
    image.last
  }
  def matches(line1: Array[Char], line2: Array[Char]): Boolean = {
    var i = 0
    while (i < l) {
      val c1 = line1(i)
      val c2 = line2(i)
      if (c1 != c2) return false
      i += 1
    }
    true
  }
  def imagesMatchingPos(x: Int, y: Int): Array[(Long, Array[Array[Char]])] = {
    val resultPos = y * gridL + x
    assert(result(resultPos) == null)
    assert(resultID(resultPos) == -1L)
    val res = scala.collection.mutable.ArrayBuffer[(Long, Array[Array[Char]])]()
    val available = images.keySet.filter( id => !resultID.contains(id) )
    for (id <- available) {
      val image = images(id)
      for (o <- orientations(image)) {
        if (x > 0 && y > 0) {
          val leftImage = result(y * gridL + (x-1))
          val topImage = result((y-1) * gridL + x)
          if (matches(getLeftCol(o), getRightCol(leftImage)) && matches(getTopRow(o), getBotRow(topImage))) res += ((id, o))
        } else if (x > 0) {
          val leftImage = result(y * gridL + (x-1))
          if (matches(getLeftCol(o), getRightCol(leftImage))) res += ((id, o))
        } else if (y > 0) {
          val topImage = result((y-1) * gridL + x)
          if (matches(getTopRow(o), getBotRow(topImage))) res += ((id, o))
        } else {
          res += ((id, o))
        }
      }
    }
    res.toArray
  }
  def inner(x: Int, y: Int): Boolean = {
    val resultPos = y * gridL + x
    if (resultPos >= result.length) return true
    val possibilities = imagesMatchingPos(x,y)
    for ((id, o) <- possibilities) {
      resultID(resultPos) = id
      result(resultPos) = o
      val nextX = if (x == gridL - 1) 0 else x + 1
      val nextY = if (x == gridL - 1) y + 1 else y
      if (inner(nextX, nextY)) return true
      resultID(resultPos) = -1L
      result(resultPos) = null
    }
    return false
  }
  
  val res = inner(0,0)
  assert(res)
  assert(resultID.distinct.length == resultID.length)
  val idTopLeft = resultID(0)
  val idTopRight = resultID(gridL - 1)
  val idBotLeft = resultID((gridL - 1) * gridL)
  val idBotRight = resultID((gridL - 1) * gridL + gridL - 1)
  println("TL: " + idTopLeft + ", TR: " + idTopRight + ", BL: " + idBotLeft + ", BR: " + idBotRight)
  val multiplied = idTopLeft * idTopRight * idBotLeft * idBotRight
  println("Multiplied: " + multiplied)
  
  def removeBorders(image: Array[Array[Char]]): Array[Array[Char]] = {
    image.drop(1).dropRight(1).map( _.drop(1).dropRight(1) )
  }
  (0 until result.length).foreach( i => result(i) = removeBorders(result(i)) )
  
  def concatImages(image1: Array[Array[Char]], image2: Array[Array[Char]]): Array[Array[Char]] = {
    val res = scala.collection.mutable.ArrayBuffer[Array[Char]]()
    var i = 0
    while (i < image1.length) {
      res += (image1(i) ++ image2(i))
      i += 1
    }
    res.toArray
  }
  val imageRows = scala.collection.mutable.ArrayBuffer[Array[Array[Char]]]()
  for (y <- 0 until gridL) {
    var current = result(y * gridL)
    for (x <- 1 until gridL) {
      val next = result(y * gridL + x)
      current = concatImages(current, next)
    }
    imageRows += current
  }
  val combinedImage = imageRows.foldLeft(Array[Array[Char]]())( _ ++ _ )
  val imageL = combinedImage.length
  println("Image combined")
  val orients = orientations(combinedImage)
  println("Image oriented")
  println("Orientations: " + orients.length)
  def findSeamonster(image: Array[Array[Char]]): Option[Array[(Int,Int)]] = {
    val smXLen = 20
    val smYLen = 3
    var i = 0
    var count = 0
    var coords = Array[(Int,Int)]()
    while (i <= imageL - smXLen) {
      var j = 0
      while (j <= imageL - smYLen) {
        val smCoords = Array((0,1),(1,2),(4,2),(5,1),(6,1),(7,2),(10,2),(11,1),(12,1),(13,2),(16,2),(17,1),(18,0),(18,1),(19,1)).map( coord => ((i + coord._1, j + coord._2)) )
        if (smCoords.forall( coord => image(coord._2)(coord._1) == '#' )) {
          count += 1
          coords = coords ++ smCoords
        }
        j += 1
      }
      i += 1
    }
    if (count < 2) return None
    else return Some(coords)
  }
  
  println("Finding seamonsters...")
  for (o <- orients) {
    //println("Orientation begun")
    val monster = findSeamonster(o)
    if (monster.isDefined) {
      println("Monsters found!")
      val smCoords = monster.get
      val notSeamonster = (for (x <- 0 until imageL; y <- 0 until imageL; if(!smCoords.contains((x,y)))) yield (x,y))
      val res = notSeamonster.count( coord => o(coord._2)(coord._1) == '#' )
      println("Water roughness: " + res)
    }
  }
}