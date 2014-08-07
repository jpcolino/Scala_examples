import scala.util.Random
import scala.collection.mutable.ArrayBuffer

val days = 100

val market_price = List.fill(days)(20*Random.nextDouble())
val contract_price = List.fill(days)(5*Random.nextDouble())
val period_size = 1::List.fill(1000)(Random.nextInt(30)).filter(x => x>0)
val period_place1 = List((for (i <- 1 to period_size.length) yield period_size.take(i).sum).toList).flatten
val period_place = List(period_place1.filter(t => t < days ).toList, List(days+1)).flatten


val differential:List[Double] = market_price.zip(contract_price).map(t =>{if(t._1 - t._2 < 0) 0 else t._1 - t._2 }):::List(1.0)

val max_position = ArrayBuffer[Int]()

for (i <- 0 to period_place.indices.last-1) {
  val partial_diff =  if(i==0)(period_place(i) to period_place(i+1)-1).map(t => differential(t-1)).toList
  else (period_place(i) to period_place(i+1)-1).map(t => differential(t-1)).toList
  val maximo = partial_diff.zipWithIndex.filter(x=>x._1>0).maxBy(_._1)._2
  if (i > 0) max_position += maximo + period_place(i)-1 else  max_position += maximo
}

val ref6a:ArrayBuffer[Double] = ArrayBuffer.fill(market_price.size)(0)
for (i <- 0 to max_position.size-1){ ref6a(max_position(i))=50 }
val ref:List[Double] = List((0 to ref6a.length-1).map(x=>ref6a(x)).toList).flatten

println(ref)

