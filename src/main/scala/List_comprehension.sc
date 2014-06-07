

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
for (i <- 1 to (mylist.length-1)) println(mylist(i))




mylist.reverse
mylist.foreach(println)





mylist.sum
mylist.take(mylist.length).sum
mylist.product
mylist indexOf(4,3)
val abcde = List('a', 'b', 'c', 'd', 'e')
abcde.last
abcde.init
abcde.reverse
abcde take 2
abcde drop 2
abcde splitAt 2
abcde(2)
val myList1 = (for (i <- 1 to mylist.length) yield mylist.take(i).sum).toList
val mylist2 = List ("a","b","c")
val mylist3 = mylist ++ mylist2// concatenation of lists
val myList4: List[Int] = 1 until 10 toList
val myList5: List[Int] = (1 until 10).toList
val myList6 = List.range(1,10)
val myList7: List[Int] = myList6 updated(8,100)
val myList8 = 1::2::4::5::Nil
val myList9 = myList7::myList8
// -----------------------------------------
// Lists with some map and filter
// -----------------------------------------
val x = List(1,2,3,4,5,6,7,8,9,10)
val myList10 = x.filter(a => a % 2 == 0)
val myList11 = x.filter(a => a % 2 == 1)
val myList12 = x.map(a => a+1)
val myList13 = x.map(a => a*a)
// -----------------------------------------
// Converting a List in an Array
// -----------------------------------------
val myArray3: Array[String] = (mylist3 map (_.toString)).toArray
println(myArray3)

// -----------------------------------------
// List of List[List[(Int)(Int)]]: an interesting example
// -----------------------------------------
val i = List.range(1,100,7)
val j = List.range(8,100,7)
val weeks = for {x<- List.range(0,i.length-1)}
  yield List.fill(7)(i(x)) zip List.fill(7)(j(x))
// -----------------------------------------
// Processing multiples lists together (zip)
// -----------------------------------------
abcde
abcde.indices
abcde.indices zip abcde
val zipped = abcde zip List(1, 2, 3)
zipped.unzip
abcde.zipWithIndex
abcde.toString()
(List(10,20),List(3,4,5)).zipped.map(_ * _)
println(List.fill(3)("Hello") zip List.fill(3)("World"))

// -----------------------------------------
// mkstring: very useful!!
// -----------------------------------------
abcde mkString ("[", ",", "]")
abcde mkString ""
abcde.mkString
(abcde mkString("\nSequence of Char\n" , "\n" , " ")).stripMargin






// -----------------------------------------
// List of Random Numbers
// -----------------------------------------
import scala.util.Random
val mylist14 = List.fill(10)(Random.nextInt(10))
val mylist15:List[Double] = List.fill(10)(Random.nextGaussian())
val mylist16:List[Double] = List.fill(10)(Random.nextDouble())filter(x=>x>0)
//A list that is the sum of each element before (a kind of cumsum in the list)
val mylist17:List[Double] = (for (i <- 1 to mylist15.length) yield mylist15.take(i).sum).toList
// -----------------------------------------
// Random Numbers Full-version for a list:
// -----------------------------------------
val mylist18:List[Double] = (for (i <- 1 to 10) yield List.fill(10)(Random.nextGaussian()).take(i).sum).toList
val mylist19:List[Double] = (for (i <- 1 to 10) yield List.fill(10)(Random.nextGaussian()).take(i).sum).toList
// identify position where une list value is bigger than the other:
val myList20 = (for {i <- 0 to mylist19.length-1 if mylist19(i)<mylist18(i)} yield i).toList
// -----------------------------------------
// List comprehension in Scala
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


