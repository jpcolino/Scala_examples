
// Scala First Steps: Several examples in Scala
//
// All the possible stupid 'Hello World'
//----------------------------------------------------


// first
println("Hello world")


//second
val hello = "hello world"
println (hello*10)


//third
val msg = "Hello, world!"
val msg2: java.lang.String = "Hello again, world!"
val msg3: String = "Hello yet again, world!"
println(msg)

println(msg2)

println(msg3)


//fourth
val greetStrings = new Array[String](3)
greetStrings(0) = "Hello"
greetStrings(1) = ", "
greetStrings(2) = "world!\n"























for (i <- 0 to 2)
  print(greetStrings(i))

// FUNCTIONS EXAMPLES
//----------------------------------------------------

// Some Basic Examples
def square(x: Double): Double = x * x
square(2)
square(5+2)
square(square(4))
def sumOfSquares(x: Double, y: Double):Double = square(x) + square(y)
sumOfSquares(3, 2 + 2)
def abs(x:Double): Double = if (x>=0) x else -x

abs(10)
abs(-10)
abs(10)==abs(-10)
def sumInt(a:Int, b:Int): Int =
  if (a > b) 0 else a + sumInt( a + 1, b)
def cube(x:Int): Int = x * x * x

def sumCubes(a:Int, b:Int): Int =
  if (a > b) 0 else cube(a) + sumCubes( a + 1, b)
// First function
// --------------------------------
// 1.1 Recursive version
// ---------------------
def factorial(n: Int): Int =
  if (n == 0) 1 else n * factorial(n - 1)
println(factorial(4))



// 1.2 Tail-recursive version
// --------------------------
def factorial2(n:Int): Int = {
  def loop(acc: Int, n: Int): Int =
    if (n==0) acc
  else loop(acc * n, n - 1)
  loop(1,n)
}
println(factorial2(3))

// 1.3 Another example involving factorial functions
// --------------------------
def sumFactorials(a:Int, b:Int): Int =
  if (a > b) 0
  else factorial(a) + sumFactorials( a + 1, b)
println(sumFactorials(3, 4) )



// 1.4. Previous example using
// Higher-Order Functions from
// FUNCTIONAL PROGRAMMING
def sum(f:Int => Int, a:Int, b:Int): Int =
  if (a > b) 0
  else f(a) + sum(f, a+1, b)
def sumFactorials2(a: Int, b: Int) = sum(factorial, a, b)
println(sumFactorials2(3, 4) )



// Second function
// --------------------------------
def max(x: Int, y: Int): Int = {
  if (x > y) x
  else y
}
max(3, 5)
// or
def max2(x: Int, y: Int) = if (x > y) x else y
max2(3, 5)
// Third function: Two implementations of Quick Sort
// --------------------------------
// 3. 1. Quick sort, imperative style
object sort {
  /** Nested methods can use and even update everything
    *  visible in their scope (including local variables or
    *  arguments of enclosing methods) which allows us to
    *  prevent namespace pollution.
    */
  def sort(a: Array[Int]) {
    def swap(i: Int, j: Int) {
      val t = a(i); a(i) = a(j); a(j) = t
    }
    def sort1(l: Int, r: Int) {
      val pivot = a((l + r) / 2)
      var i = l
      var j = r
      while (i <= j) {
        while (a(i) < pivot) i += 1
        while (a(j) > pivot) j -= 1
        if (i <= j) {
          swap(i, j)
          i += 1
          j -= 1
        }
      }
      if (l < j) sort1(l, j)
      if (j < r) sort1(i, r)
    }

    if (a.length > 0)
      sort1(0, a.length - 1)
  }

  def println(ar: Array[Int]) {
    def print1 = {
      def iter(i: Int): String =
        ar(i) + (if (i < ar.length-1) "," + iter(i+1) else "")
      if (ar.length == 0) "" else iter(0)
    }
    Console.println("[" + print1 + "]")
  }

  def main(args: Array[String]) {
    val ar = Array(6, 2, 8, 5, 1)
    println(ar)
    sort(ar)
    println(ar)
  }
}
/** 3. 2 Quick sort, functional style */
object sort1 {
  def sort(a: List[Int]): List[Int] = {
    if (a.length < 2)
      a
    else {
      val pivot = a(a.length / 2)
      sort(a.filter(_ < pivot)) :::
        a.filter(_ == pivot) :::
        sort(a.filter(_ > pivot))
    }
  }
  def main(args: Array[String]) {
    val xs = List(6, 2, 8, 5, 1)
    println(xs)
    println(sort(xs))
  }
}
// Fourth function
// --------------------------------
// Very silly function to check prime numbers
def isPrime(n: Int) = (2 until n) forall (n % _ != 0)
for (i <- 1 to 100 if isPrime(i))
println(i)


















































































println(isPrime(100))






// Fifth Function
//---------------------------------------------
// Square Roots by Newton's method of successive approximations
// nesting functions inside main block function
def sqrt(x: Double) = {
  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)
  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2
  def isGoodEnough(guess: Double, x: Double) =
    abs(square(guess) - x) < 0.001
  sqrtIter(1.0, x)
}
println(sqrt(9))





// Sixth Function
//---------------------------------------------
// Function that compute the
// Greatest Common Divider (gcd)
// of two numbers integers a and b
def gcd(a:Int, b:Int): Int =
if (b==0) a else gcd(b, a%b)
println (gcd(10,12))


println (gcd(100,120))






// ----------------------------------------------------
// Examples of traits and classes
// --------------------------------
trait Philosophical {
  def philosophize() {
    println("I consume memory, therefore I am!")
  }
}
/** class Frog extends Philosophical {
  override def toString = "green"
}*/
class Animal
class Frog extends Animal with Philosophical {
  override def toString = "green"
}
val frog = new Frog
val phil: Philosophical = frog
frog.philosophize()


phil.philosophize()




// Some Examples of Functions as Objects
// -----------------------------------------

/**

Remember that function values are treated as objects in Scala.

The functions type A => B is just an abbreviation for the class
scala.Function1[A,B] which is roughly defined as

package scala
trait Function1[A,B]{
  def apply (x:A):B
}

Functions are objects with apply methods
*/


val f = (x:Int)=>x*x // that is an eta-expansion
println(f(7))

/** is equivalent to
{class AnonFun extends Functions1[Int, Int]{
  def apply(x:Int) = x*x
  }
  new AnonFun
}

 or shorter using anonymous class syntax:

*/

val f2 = new Function1[Int,Int]{
  def apply(x:Int)=x*x
}

println(f2.apply(7))

// List comprehension in scala
// -----------------------------------------
object ComprehensionTest1 {
  for (i <- Iterator.range(0, 20);
       j <- Iterator.range(i + 1, 20) if i + j == 32)
    println("(" + i + ", " + j + ")")
}

println(ComprehensionTest1)




object ComprehensionTest2 {
  for (i <- Iterator.range(0, 50);
       j <- Iterator.range(50, 100))
    println("(" + i + ", " + j + ")")
}

println(ComprehensionTest2)




































