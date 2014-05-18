import java.util.concurrent.ThreadLocalRandom
import scala.math.exp
import scala.annotation.tailrec

object MonteCarlo {

  @tailrec
  def sum(its: Long,acc: Double): Double = {
    if (its==0)
      (acc)
    else {
      val u=ThreadLocalRandom.current().nextDouble()
      sum(its-1,acc+exp(-u*u))
    }
  }

  def main(args: Array[String]) = {
    println("Hello")
    val iters=1000000000
    val result=sum(iters,0.0)
    println(result/iters)
    println("Goodbye")
  }

}