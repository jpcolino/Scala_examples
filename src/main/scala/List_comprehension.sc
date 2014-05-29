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
       j <- Iterator.range(0, 50) if i+5 > j )
       println ("(" + i + ", " + j + ")")
}

println(ComprehensionTest2)




































val factors  =  ( n : Int )  =>  for(  x <- ( 1 to n );  if n % x == 0 )  yield x

println(factors(100))

