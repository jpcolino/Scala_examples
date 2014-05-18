/** Something to work on...
  * code from http://patricknicolas.blogspot.de/2013/07/a-fast-black-scholes-simulator.html
  * a nice example to study
  */



/**
def callPrice(S: Double, X: Double, r: Double, sigma: Double, T: Double): Option[Double]= {
  import org.apache.commons.math3.analysis.function.Gaussian

  val d1 = (Math.log(S/X) + (r + 0.5*sigma * sigma) * T) / (sigma * Math.sqrt(T))
  val d2 = d1 - sigma * Math.sqrt(T)
  val gauss = new GaussianSome(S* gauss.value(d1 - X * Math.exp(-r * T) *gauss.value(d2))
}
  */


case class BlackScholesStep(val f: (Double*) => Double, var c: Double = 0.0) {
  def update(x:Double, y:Double) : Unit = { c = f(x, y) }
  def update(x:Double, y:Double, z:Double) : Unit = {c = f(x, y, z) }
}

def bs1f(x: Double*): Double = Math.log(x(0)/x(1))
val bs1 = new BlackScholesStep(bs1f)

def bs2f(x: Double*): Double = x(0)*x(1)
val bs2 =  new BlackScholesStep(bs2f)

def bs3f(x: Double*): Double = 0.5*x(0)*x(0)*x(1)
val bs3 =  new BlackScholesStep(bs3f)

def bs4f(x: Double*): Double = x(0) * Math.sqrt(x(1))
val bs4 =  new BlackScholesStep(bs4f)

def bs5f(x: Double*): Double = - x(0) * Math.exp(-x(1)*x(2))
val bs5 =  new BlackScholesStep(bs5f)

class CallPriceSimulator(var S: Double, var X: Double, var r: Double, var sigma: Double, var T: Double) {
  import org.apache.commons.math3.analysis.function.Gaussian

  val call = () => {
    bs1.update(S,X)
    bs2.update(r,T)
    bs3.update(sigma,T)
    bs4.update(sigma,T)
    bs5.update(X,r,T)
    call_
  }

  val dS =(S: Double) => { bs1.update(S,X); call_  }
  val dSigma = (sigma: Double) =>{
    bs3.update(sigma, T)
    bs4.update(sigma, T)
    call_
  }

  private val call_ = () => {
    val gauss = new Gaussian
    val d1 = (bs1.c + bs2.c + bs3.c)/bs4.c
    S * gauss.value(d1) - bs5.c*gauss.value(d1 -bs4.c)
  }
}

def monteCarlo(S:Double, X:Double, r:Double, sigma:Double, T:Double):Option[Double]={
  import scala.util.Random

  val rGen = new Random(System.currentTimeMillis)
  val alpha1 = Math.exp(r*T)
  val alpha2 = S*Math.exp(- 0.5*sigma*sigma*T)
  val theta = alpha1*alpha2
  val delta = Math.exp( sigma*Math.sqrt(T))

  val totalValues = (0 until maxIters).scanLeft(0.0)( (sum, n) => {
    rGen.setSeed(rGen.nextLong)
    val price = theta*Math.pow(delta, rGen.nextGaussian)
    if( price < X) 0 else price - X
  }).takeWhile( _ > 0)

  if(totalValues.size < maxIters) Some(alpha1*totalValues.last/maxIters) else None
}