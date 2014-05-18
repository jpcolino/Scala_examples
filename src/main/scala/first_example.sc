
// All the possible dumbs 'Hello World'
// ----------------------------------------


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
val frog = new Frog
val phil: Philosophical = frog
println(factorial(10))
val movieSet1 = Set("Hitch", "Poltergeist")
//import scala.collection.mutable.Map
val treasureMap = Map[Int, String]()
max(3, 5)
max2(3, 5)
val greetStrings2 = new Array[String](3)
val numNames = Array("zero", "one", "two")
val numNames2 = Array.apply("zero", "one", "two")
frog.philosophize()
val oneTwoThree = List(1, 2, 3)
phil.philosophize()
val pair = (99, "Luftballons")

// Use sets and maps

import scala.collection.mutable.Set


//var jetSet = Set("Boeing", "Airbus")
//jetSet += "Lear"
//println(jetSet.contains("Cessna"))
val movieSet = Set("Hitch", "Poltergeist")
movieSet1 += "Shrek"
println(movieSet1)
val hashSet = HashSet("Tomatoes", "Chilies")
treasureMap += (1 -> "Go to island.")
treasureMap += (2 -> "Find big X on ground.")
treasureMap += (3 -> "Dig.")
println(treasureMap(2))
val treasureMap1 = Map[Int, String]()

greetStrings2.update(0, "Hello")
greetStrings2.update(1, ", ")
greetStrings2.update(2, "world!\n")

for (i <- 0.to(2))
  print(greetStrings.apply(i))
val romanNumeral = Map(
  1 -> "I", 2 -> "II", 3 -> "III", 4 -> "IV", 5 -> "V"

)
    println("numNames.toList [" + numNames.toList + "]")
val args = Array("nine", "ten")
    println("numNames2.toList [" + numNames2.toList + "]")
val res = formatArgs(Array("zero", "one", "two"))
println("oneTwoThree [" + (oneTwoThree) + "]") {
  val oneTwo = List(1, 2)
  val threeFour = List(3, 4)
  val oneTwoThreeFour = oneTwo ::: threeFour
  println("" + oneTwo + " and " + threeFour + " were not mutated.")
  println("Thus, " + oneTwoThreeFour + " is a new list.")
} {
  val twoThree = List(2, 3)
  val oneTwoThree = 1 :: twoThree
  println(oneTwoThree)
} {
      val oneTwoThree = 1 :: 2 :: 3 :: Nil
      println(oneTwoThree)

}
var jetSet = Set("Boeing", "Airbus")
println(pair._1)
println(pair._2)


// First function
// --------------------------------
def factorial(x: BigInt): BigInt =
  if (x == 0) 1 else x * factorial(x - 1)
jetSet += "Lear"
println(jetSet.contains("Cessna"))
jetSet = jetSet + "Lear"
println("jetSet [" + jetSet + "]")

// Second function
// --------------------------------
def max(x: Int, y: Int): Int = {
  if (x > y) x
  else y
}
movieSet += "Shrek"
println(movieSet)

def max2(x: Int, y: Int) = if (x > y) x else y
println(hashSet + "Coriander")


def printArgs(args: Array[String]): Unit = {
  var i = 0
  while (i < args.length) {
    println(args(i))
    i += 1
  }

}
treasureMap1 += (1 -> "Go to island.")
treasureMap1 += (2 -> "Find big X on ground.")
treasureMap1 += (3 -> "Dig.")
println(treasureMap1(2))

def formatArgs(args: Array[String]) = args.mkString("\n")

println(romanNumeral(4))

// Examples of traits and classes
// --------------------------------
trait Philosophical {
  def philosophize() {
    println("I consume memory, therefore I am!")
  }
}

printArgs(Array("zero", "one", "two")) {
  def printArgs(args: Array[String]): Unit = {
    for (arg <- args)
      println(arg)
  }
  printArgs(Array("three", "four", "five"))

} {
  def printArgs(args: Array[String]): Unit = {
    args.foreach(println)
  }
  printArgs(Array("six", "seven", "eight"))

}

class Frog extends Philosophical {
  override def toString = "green"
}

//last one
object HelloWorld {
  def main(args: Array[String]) {
    println("Hello, world!")
  }
}


println(formatArgs(args))


object Ex1 {
  def main(args: Array[String]) {
    println("new Frog [" + (new Frog) + "]")
  }

  class Animal

  class Frog extends Animal with Philosophical {
    override def toString = "green"
  }

}

assert(res == "zero\none\ntwo")

println("whew")


/** Print prime numbers less than 100, very inefficiently */
object primes extends Application {
  def isPrime(n: Int) = (2 until n) forall (n % _ != 0)

  for (i <- 1 to 100 if isPrime(i)) println(i)
}
