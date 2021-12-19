package day18

import scala.io.Source
import scala.collection.mutable.Stack

class SnailNumber {
  var left: Either[Long, SnailNumber] = Left(0L)
  var right: Either[Long, SnailNumber] = Left(0L)
  var parent: SnailNumber = null

  def magnitude(): Long = 3 * left.fold[Long](l => l, l => l.magnitude()) + 2 * right.fold[Long](r => r, r => r.magnitude())

  def explode() = {
    assert(this.left.isLeft && this.right.isLeft)
    var prev = this
    var rightestLeft = this.parent
    while (rightestLeft != null && rightestLeft.left.isRight && rightestLeft.left.toOption.get == prev) {
      prev = rightestLeft
      rightestLeft = rightestLeft.parent
    }
    if (rightestLeft != null) {
      if (rightestLeft.left.isRight) {
        rightestLeft = rightestLeft.left.toOption.get
        while (rightestLeft.right.isRight) rightestLeft = rightestLeft.right.toOption.get
        rightestLeft.right = Left(rightestLeft.right.swap.toOption.get + this.left.swap.toOption.get)
      } else {
        rightestLeft.left = Left(rightestLeft.left.swap.toOption.get + this.left.swap.toOption.get)
      }
    }

    prev = this
    var leftestRight = this.parent
    while (leftestRight != null && leftestRight.right.isRight && leftestRight.right.toOption.get == prev) {
      prev = leftestRight
      leftestRight = leftestRight.parent
    }
    if (leftestRight != null) {
      if (leftestRight.right.isRight) {
        leftestRight = leftestRight.right.toOption.get
        while (leftestRight.left.isRight) leftestRight = leftestRight.left.toOption.get
        leftestRight.left = Left(leftestRight.left.swap.toOption.get + this.right.swap.toOption.get)
      } else {
        leftestRight.right = Left(leftestRight.right.swap.toOption.get + this.right.swap.toOption.get)
      }
    }
    
    if (this.parent.left.isRight && this.parent.left.toOption.get == this) this.parent.left = Left(0L)
    else this.parent.right = Left(0L)
    this.parent = null
  }
  
  def split() = {
    if (this.left.isLeft && this.left.swap.toOption.get >= 10) {
      val newLeft = new SnailNumber
      val thisLeft = this.left.swap.toOption.get
      newLeft.left = Left(thisLeft / 2)
      newLeft.right = Left((thisLeft + 1) / 2)
      newLeft.parent = this
      this.left = Right(newLeft)
    } else {
      val newRight = new SnailNumber
      val thisRight = this.right.swap.toOption.get
      newRight.left = Left(thisRight / 2)
      newRight.right = Left((thisRight + 1) / 2)
      newRight.parent = this
      this.right = Right(newRight)
    }
  }
  
  override def toString() = {
    val l = this.left.fold(l => l.toString(), l => l.toString())
    val r = this.right.fold(r => r.toString(), r => r.toString())
    "[" + l + "," + r + "]"
  }
}
class Frame(val sn: SnailNumber) {
  var leftTraversed = false
  var rightTraversed = false
}

object Snailfish extends App {
  def indexOfSplit(s: String): Int = {
    var i = 0
    while (s(i) != '[') i += 1
    if (s.take(i).contains(',')) return i - 1
    var count = 1
    i += 1
    while (count > 0) {
      if (s(i) == '[') count += 1
      else if (s(i) == ']') count -= 1
      i += 1
    }
    return i
  }
  def getNumber(n: String): SnailNumber = {
    val root = new SnailNumber
    val nn = n.tail.dropRight(1)
    if (!nn.contains('[')) {
      val (l, r) = nn.splitAt(nn.indexOf(','))
      root.left = Left(l.toLong)
      root.right = Left(r.tail.toLong)
      return root
    }
    val i = indexOfSplit(nn)
    val (l, r) = nn.splitAt(i)
    if (l.contains('[')) root.left = Right(getNumber(l))
    else root.left = Left(l.toLong)
    if (r.tail.contains('[')) root.right = Right(getNumber(r.tail))
    else root.right = Left(r.tail.toLong)
    root.left.foreach(_.parent = root)
    root.right.foreach(_.parent = root)
    return root
  }

  val s = Source.fromFile("src/day18/input.txt")
  val strings = s.getLines().toArray
  s.close()
  val numbers = strings.map(getNumber(_))

  def canExplode(root: SnailNumber, layer: Int): Option[SnailNumber] = {
    if (layer > 4 && root.left.isLeft && root.right.isLeft) return Some(root)

    val left = root.left.fold[Option[SnailNumber]](l => None, l => canExplode(l, layer + 1))
    if (left.isDefined) return left

    val right = root.right.fold[Option[SnailNumber]](r => None, r => canExplode(r, layer + 1))
    return right
  }
  def canSplit(root: SnailNumber): Option[SnailNumber] = {
    val stack = Stack[Frame](new Frame(root))
    while (stack.nonEmpty) {
      val current = stack.top
      if (!current.leftTraversed) {
        current.leftTraversed = true
        if (current.sn.left.isLeft && current.sn.left.swap.toOption.get >= 10) return Some(current.sn)
        if (current.sn.left.isRight) stack.push(new Frame(current.sn.left.toOption.get))
      } else if (!current.rightTraversed) {
        current.rightTraversed = true
        if (current.sn.right.isLeft && current.sn.right.swap.toOption.get >= 10) return Some(current.sn)
        if (current.sn.right.isRight) stack.push(new Frame(current.sn.right.toOption.get))
      } else {
        stack.pop()
      }
    }
    return None
  }
  def addition(n1: SnailNumber, n2: SnailNumber): SnailNumber = {
    val root = new SnailNumber
    n1.parent = root
    n2.parent = root
    root.left = Right(n1)
    root.right = Right(n2)
    //println(root.toString())
    var finished = false
    while (!finished) {
      val exploder: Option[SnailNumber] = canExplode(root, 1)
      val splitter: Option[SnailNumber] = canSplit(root)
      if (exploder.isDefined) {
        //println(exploder.get.toString())
        exploder.get.explode()
        //println("After explosion: " + root.toString())
      } else if (splitter.isDefined) {
        //println(splitter.get.toString())
        splitter.get.split()
        //println("After split: " + root.toString())
      } else {
        finished = true
      }
    }
    return root
  }

  val res = numbers.tail.foldLeft(numbers(0))(addition(_, _))
  val resMagnitude = res.magnitude()
  println("Magnitude of final sum: " + resMagnitude)
  
  var maxMagnitude = 0L
  for (n1 <- strings) {
    for (n2 <- strings) {
      if (n1 != n2) {
        val magnitude = addition(getNumber(n1),getNumber(n2)).magnitude()
        if (magnitude > maxMagnitude) maxMagnitude = magnitude
      }
    }
  }
  println("Largest magnitude of two numbers: " + maxMagnitude)
}