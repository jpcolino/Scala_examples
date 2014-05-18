
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

// Basic Examples
def square(x: Double) = x * x
square(2)
square(5+2)
square(square(4))

def abs(x:Double) = if (x>=0) x else -x

abs(10)
abs(-10)
abs(10)==abs(-10)

// First function
// --------------------------------
def factorial(x: BigInt): BigInt =
  if (x == 0) 1 else x * factorial(x - 1)
println(factorial(10))



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
    *  arguments of enclosing methods).
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







// Some Examples of Data Types
// -----------------------------------------
