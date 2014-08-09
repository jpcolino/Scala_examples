trait Similarity {
  def isSimilar(x: Any): Boolean
  def isNotSimilar(x: Any): Boolean = !isSimilar(x)
}

/**
 *
 * This trait consists of two methods isSimilar and isNotSimilar.
 * While isSimilar does not provide a concrete method implementation
 * (it is abstract in the terminology of Java), method isNotSimilar
 * defines a concrete implementation.
 * Consequently, classes that integrate this trait only have
 * to provide a concrete implementation for isSimilar.
 * The behavior for isNotSimilar gets inherited
 * directly from the trait.
 * Traits are typically integrated into a class
 */

class Point(xc: Int, yc: Int) extends Similarity {
  var x: Int = xc
  var y: Int = yc

  def isSimilar(obj: Any) =
    obj.isInstanceOf[Point] &&
      obj.asInstanceOf[Point].x == x

  def move(dx: Int, dy: Int) {
    x = x + dx
    y = y + dy
  }
  override def toString(): String = "(" + x + ", " + y + ")";
}

/**
 *
 * The class defines two variables x and y and three methods:
 *  isSimilar, move and toString.
 *  move takes two integer arguments but does not return a value
 *  (the implicit return type Unit corresponds to void in Java-like
 *  languages).
 *  toString, on the other hand, does not take
 *  any parameters but returns a String value.
 *
 *  Since toString overrides the pre-defined toString method,
 *  it has to be tagged with the override flag.

 */

object TraitsTest extends App {
  val p1 = new Point(2, 3)
  val p2 = new Point(2, 4)
  val p3 = new Point(3, 3)
  println(p1.isNotSimilar(p2))
  println(p1.isNotSimilar(p3))
  println(p1.isNotSimilar(2))
}


object Classes {
  def main(args: Array[String]) {
    val pt = new Point(1, 2)
    println(pt)
    pt.move(10, 10)
    println(pt)
  }
}

