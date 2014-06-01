

// Basics about lists
// -----------------------------------------
val mylist = List(1,2,3,4,5)
mylist.length
mylist.head
mylist.last
mylist.init
mylist.tail
mylist take 3
mylist drop 3
mylist(0)
mylist.reverse
mylist.foreach(println)





mylist indexOf(4,3)
val mylist2 = List ("a","b","c")
val mylist3 = mylist ++ mylist2// concatenation of lists
val myList4: List[Int] = 1 until 10 toList
val myList5: List[Int] = (1 until 10).toList
val myList6 = List.range(1,10)
val myList7: List[Int] = myList6 updated(8,100)
val myList8 = 1::2::4::5::Nil
val myList9 = myList7::myList8
// Lists with some map and filter
// -----------------------------------------
val x = List(1,2,3,4,5,6,7,8,9,10)
val myList10 = x.filter(a => a % 2 == 0)
val myList11 = x.filter(a => a % 2 == 1)
val myList12 = x.map(a => a+1)
val myList13 = x.map(a => a*a)


// Converting a List in an Array
// -----------------------------------------
val myArray3: Array[String] = (mylist3 map (_.toString)).toArray
println(myArray3)

// List of random numbers
// -----------------------------------------
import scala.util.Random
val mylist14 = List.fill(10)(Random.nextInt(10))
val mylist15:List[Double] = List.fill(10)(Random.nextGaussian())filter(x=>x>0)
val mylist16:List[Double] = List.fill(10)(Random.nextDouble())

private val volatility = 0.5
val mylist17:List[Double] = List.fill(10)(Random.nextGaussian())

// List comprehension in scala
// -----------------------------------------
object ComprehensionTest1 {
  for (i <- Iterator.range(0, 20);
       j <- Iterator.range(i + 1, 20) if i + j == 32)
    println("(" + i + ", " + j + ")")
}
println(ComprehensionTest1)





object ComprehensionTest2 extends App {
  for (i <- Iterator.range(0, 20);
       j <- Iterator.range(i, 20) if i + j == 32)
    println("(" + i + ", " + j + ")")
}
val factors  =  ( n : Int )  =>  for(  x <- ( 1 to n );  if n % x == 0 )  yield x
println(factors(100))


