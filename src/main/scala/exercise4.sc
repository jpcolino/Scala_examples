/**
 * Polymorphic example in Scala
 * Method dup in object PolyTest is parameterized with type T
 * and with the value parameters x: T and n: Int.
 *
 * When method dup is called, the programmer provides
 * the required parameters (see line 8 in the program above),
 * but as line 9 in the program above shows,
 * the programmer is not required to give actual
 * type parameters explicitly. The type system of Scala
 * can infer such types. This is done by looking at the types
 * of the given value parameters and at the context where
 * the method is called.
 *
 */
object PolyTest extends App {
  def dup[T](x: T, n: Int): List[T] =
    if (n == 0)
      Nil
    else
      x :: dup(x, n - 1)
  println(dup[Int](3, 4))
  println(dup("three", 3))
}