
class Point(xc: Int, yc: Int) {
  var x: Int = xc
  var y: Int = yc

  def move(dx: Int, dy: Int) {
    x = x + dx
    y = y + dy
  }
  override def toString(): String = "(" + x + ", " + y + ")";
}

/**
 *
 * The class defines two variables x and y and two methods:
 *  move and toString.
 *  move takes two integer arguments but does not return a value
 *  (the implicit return type Unit corresponds to void in Java-like
 *  languages).
 *  toString, on the other hand, does not take
 *  any parameters but returns a String value.
 *
 *  Since toString overrides the pre-defined toString method,
 *  it has to be tagged with the override flag.

 */


object Classes {
  def main(args: Array[String]) {
    val pt = new Point(1, 2)
    println(pt)
    pt.move(10, 10)
    println(pt)
  }
}

