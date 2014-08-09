import scala.annotation.tailrec
import scala.util.Random
import scala.math._

/**
 * Matrix is a mutable matrix class.
 *
 * @note An array is used internally for performance reasons.
 *
 * @constructor Create a new m x n Matrix.
 *
 * @param m The number of rows.
 * @param n The number of columns.
 */
class Matrix(m: Int, n: Int) { //todo make this generic for other types than doubles
val rows = m
  val cols = n
  private val mArray = Array.ofDim[Double](rows, cols)

  /**
   * @constructor Create a new m x 1 Matrix.
   *
   * @param m The number of rows.
   */
  def this(m: Int) = this(m, 1)

  /**
   * @constructor Create a new Matrix from inArray.
   *
   * @param The number of rows.
   */
  def this(inArray: Array[Array[Double]]) = {
    this(inArray.length, inArray(0).length)
    var i = 0
    while (i < rows) {
      Array.copy(inArray(i), 0, mArray(i), 0, cols)
      i += 1
    }
  }

  def apply(i: Int, j: Int): Double = {
    mArray(i)(j)
  }

  def update(i: Int, j: Int, elem: Double) {
    mArray(i)(j) = elem
  }

  def update(i: Int, row: Array[Double]) {
    mArray(i) = row
  }

  def *(b: Matrix): Matrix = {
    assert (cols == b.rows)
    val p = b.cols
    val ab = new Matrix(rows, p)
    var i = 0
    while (i < rows) {
      var j = 0
      while (j < p) {
        var acc = 0.0
        var k = 0
        while (k < cols) {
          acc += this(i, k)*b(k, j)
          k += 1
        }
        ab(i, j) = acc
        j += 1
      }
      i += 1
    }
    ab
  }

  /**
   * The method divideRowByC transforms the matrix by dividing a row by a scalar
   * @param row The row that will be operated on.
   * @param c The scalar.
   */
  private def divideRowByC(row: Int, c: Double) = {
    mArray(row).transform(x => (x/c + 0.0))
  }

  /**
   * The method addRowACtoB transforms the matrix
   * by adding a row multiplied by a scalar to another row.
   * @param a The row that will be added to the target. (unmodified by the method)
   * @param c The scalar.
   * @param b The target row that will be operated on.
   */
  private def addRowACtoB(a: Int, c: Double, b: Int) = {
    var j = 0
    while (j < cols) {
      this(b, j) = (this(b, j) + this(a, j)*c) + 0.0
      j += 1
    }
  }

  /**
   * The method swapRows transforms the matrix by swapping rows a and b.
   */
  private def swapRows(a: Int, b: Int) {
    val temp = mArray(a)
    mArray(a) = mArray(b)
    mArray(b) = temp
  }

  override def toString(): String = {
    val strB = new StringBuilder
    strB.append("[")
    var i = 0
    while (i < rows) {
      var j = 0
      while (j < cols) {
        strB.append("% 6.2f".format(this(i, j)))
        strB.append(" ")
        j += 1
      }
      if (i < rows-1)
        strB.append("\n ")
      i += 1
    }
    strB.append("]\n")
    strB.result
  }

}

object Matrix {

  private def identity(m: Int, n: Int): Matrix = {
    assert(m == n)
    val idMatrix = new Matrix(m, n)
    var idx = 0
    while (idx < m) {
      idMatrix(idx, idx) = 1.0D
      idx += 1
    }
    idMatrix
  }

  /**
   * Generate the inverse matrix using Gaussian-Jordan elimination.
   */
  val gjInverse = (matrix: Matrix) => {
    val gjMatrix = new Matrix(matrix.mArray)
    val invMatrix = Matrix.identity(matrix.rows, matrix.cols)

    var pivot = 0
    while ((pivot < matrix.rows) && (pivot < matrix.cols)) {
      var i = pivot
      var maxElem = gjMatrix(i, pivot)
      var j = pivot
      while (j < matrix.rows) {
        if (gjMatrix(j, pivot) > maxElem) {
          i = j
          maxElem = gjMatrix(j, pivot)
        }
        j += 1
      }
      if (pivot != i) {
        gjMatrix.swapRows(pivot, i)
        invMatrix.swapRows(pivot, i)
      }
      val c1 = gjMatrix(pivot, pivot)
      gjMatrix.divideRowByC(pivot, c1)
      invMatrix.divideRowByC(pivot, c1)
      var k = 0
      while (k < matrix.rows) {
        if (k != pivot) {
          val c2 = -gjMatrix(k, pivot)
          gjMatrix.addRowACtoB(pivot, c2, k)
          invMatrix.addRowACtoB(pivot, c2, k)
        }
        k += 1
      }
      pivot = pivot+1
    }
    invMatrix
  }

  /**
   * Generate the transpose Matrix.
   */
  val transpose = (matrix: Matrix) => {
    val tMatrix = new Matrix(matrix.cols, matrix.rows)
    var i = 0
    while (i < matrix.rows) {
      var j = 0
      while (j < matrix.cols) {
        tMatrix(j, i) = matrix(i, j)
        j += 1
      }
      i += 1
    }
    tMatrix
  }

}

/**
 * LsmParams is container for the
 * least squares Monte Carlo algorithm parameters.
 *
 * @param numPaths The number of simulation paths.
 * @param expiry The number of years until expiry.
 * @param numSteps The number of exercise steps per year.
 * @param stock The price of the underlying stock.
 * @param strike The exercise price.
 * @param rate The interest rate.
 * @param volatility The volatility of returns.
 *
 * @note dT = expiry / numSteps
 */
case class LsmParams( isPut: Boolean,
                      numPaths: Int,
                      expiry: Int,
                      numSteps: Int,
                      stock: Double,
                      strike: Double,
                      rate: Double,
                      volatility: Double
                      ) {

  /**
   * dT = expiry / numSteps
   */
  val dT: Double = expiry.toDouble / numSteps.toDouble

  override def toString(): String = {
    val strB = new StringBuilder
    strB.append("[")
    val formatStr = "% .3f"
    strB.append(" numPaths: "+numPaths)
    strB.append(" numSteps: "+numSteps)
    strB.append(" dT: "+(formatStr.format(dT)))
    strB.append(" expiry: "+expiry)
    strB.append(" stock: "+(formatStr.format(stock)))
    strB.append(" strike: "+(formatStr.format(strike)))
    strB.append(" rate: "+(formatStr.format(rate)))
    strB.append(" volatility: "+(formatStr.format(volatility)))
    strB.append(" ]")
    strB.result
  }
}

object lsm {
  import Matrix._

  private val DEBUG = false

  private val rng: Random = new Random
  private val sqr = (x: Double) => (x * x)

  /**
   * Generate a matrix of simulated stock prices from the lsm parameters.
   *
   * @param params The lsm parameters.
   *
   * @note Half of the paths are antithetic
   */
  val genPriceMatrix = (params: LsmParams) => {
    assert( params.numPaths%2 == 0)
    val n = params.numSteps+1
    val pMatrix = new Matrix(params.numPaths, n)
    val a = (params.rate -  sqr(params.volatility)*0.5)*params.dT
    val b = params.volatility*sqrt(params.dT)
    var i = 0
    while (i < params.numPaths/2) {
      pMatrix(i*2, 0) = params.stock
      pMatrix(i*2+1, 0) = params.stock
      var s1 = params.stock
      var s2 = params.stock
      var j = 1
      while (j < n) {
        val dZ = rng.nextGaussian
        s1 = s1*exp(a + b*dZ)
        s2 = s2*exp(a - b*dZ) // antithetic path
        pMatrix(i*2, j) = s1
        pMatrix(i*2+1, j) = s2
        j += 1
      }
      i += 1
    }
    pMatrix
  }

  /**
   * Regress y onto the basis function applied to current stock prices.
   *
   * @param fnX The results of applying the basis function to current stock prices.
   * @param y The discounted cash flows received from continuing.
   *
   * @note The current algorithm (calculating the inverse
   *       matrix using gaussian elimination) might not be
   *       the most suitable one for this task.
   */
  private val regress = (fnX: Matrix, y: Matrix) => {
    val transposeFnX = transpose(fnX)
    val inverse = gjInverse(transposeFnX*fnX)
    val coeff = inverse*transposeFnX*y
    fnX*coeff
  }

  /**
   * Calculate the optimum decision (exercise or continue to hold)
   * for the current step and update the cash flow matrix to reflect this.
   *
   * @param step The current step (instant in time)
   * @param fn The basis function.
   * @param params The lsm parameters.
   * @param priceMatrix The stock price matrix.
   * @param cfMatrix The cash flow matrix.
   *
   * @note The cash flow matrix is modified by this function.
   *
   * @note The current algorithm (calculating the inverse matrix
   *       using gaussian elimination) might not be the most suitable
   *       one for this task.
   */
  val calcCFAtStep = (
                       step: Int,
                       fn: (Double) => Array[Double],
                       params: LsmParams,
                       priceMatrix: Matrix,
                       cfMatrix: Matrix
                       ) => {
    val exerciseFn = (idx: Int, step: Int) => (params.strike - priceMatrix(idx, step))

    if (step == params.numSteps) {
      var i = 0
      while (i < params.numPaths) {
        if (exerciseFn(i, step) > 0)
          cfMatrix(i, step-1) = exerciseFn(i, step)
        i += 1
      }
      if (DEBUG) {
        println("cfMatrix\n"+cfMatrix)
      }
      cfMatrix
    } else {
      var xySize = 0
      var i = 0
      while (i < params.numPaths) {
        if (exerciseFn(i, step) > 0)
          xySize += 1
        i += 1
      }

      //val x = new Matrix(xySize, 1)
      val y = new Matrix(xySize, 1)
      val fnX = new Matrix(xySize, fn(0).length)
      i = 0
      var k = 0
      while (i < params.numPaths) {
        if (exerciseFn(i, step) > 0) {
          //x(k, 0) =  priceMatrix(i, step)
          //fnX(k) = fn(x(k, 0))
          fnX(k) = fn(priceMatrix(i, step))
          var maxCF = 0.0
          var j = step // step-1 is current
          while (j < cfMatrix.cols) {
            maxCF = max(maxCF, cfMatrix(i, j)*exp(-params.rate*((j-(step-1))*params.dT)))
            j += 1
          }
          y(k, 0) = maxCF
          k += 1
        }
        i += 1
      }
      val contMatrix = regress(fnX, y)

      i = 0
      var j = 0
      while (i < params.numPaths) {
        if (exerciseFn(i, step) > 0) {
          cfMatrix(i, step-1) = {
            if (exerciseFn(i, step) >= contMatrix(j, 0) ) {
              k = step
              while (k < cfMatrix.cols) {
                cfMatrix(i, k) = 0
                k += 1
              }
              exerciseFn(i, step)
            } else {
              0.0 // don't exercise now
            }
          }
          j += 1
        }
        i += 1
      }
      cfMatrix
    }
  }

  /**
   * Calculate the option value and standard error from the cash flow matrix.
   *
   * @param params The lsm parameters.
   * @param cfMatrix The cash flow matrix.
   *
   */
  val optionValueStdErr = (params: LsmParams, cfMatrix: Matrix) => {
    val m = params.numPaths
    val pvArray = Array.ofDim[Double](m)

    var i = 0
    var n = 0
    var mean = 0.0D
    var m2 = 0.0D
    while (i < m) {
      var j = 0
      while (j < cfMatrix.cols) {
        if (cfMatrix(i, j) > 0)
          pvArray(i) = (cfMatrix(i, j)*exp(-params.rate*((j+1)*params.dT)))
        j += 1
      }

      val x = pvArray(i)
      n += 1
      val delta = x - mean
      mean += delta/n
      m2 += delta*(x - mean)

      i += 1
    }
    val optionValue = pvArray.sum / m
    val variance = m2/(n-1)
    val sampStdDev = sqrt(variance)
    val stdErr = sampStdDev/sqrt(m)
    if (DEBUG) {
      println("pvArray")
      prettyPrint(pvArray)
    }
    (optionValue, stdErr)
  }


  /**
   * Recurse through the cash flow matrix calculating
   * the cash flows at each time step.
   *
   * @param step The current time step.
   * @param params The lsm parameters.
   * @param priceMatrix The stock price matrix.
   * @param cfMatrix The cash flow matrix.
   * @param basisFn The basis function.
   *
   */
  @tailrec val recurseCF:(Int, LsmParams, Matrix, Matrix, (Double) => Array[Double]) => Matrix =
    (step: Int, params: LsmParams, priceMatrix: Matrix, cfMatrix: Matrix, basisFn: (Double) => Array[Double]) => {
      assert(priceMatrix.rows == params.numPaths)
      assert(priceMatrix.cols == params.numSteps+1)
      assert(cfMatrix.rows == params.numPaths)
      assert(cfMatrix.cols == params.numSteps)

      step match {
        case 1 => calcCFAtStep(1, basisFn, params, priceMatrix, cfMatrix)
        case _ => {
          val newCFMatrix = calcCFAtStep(step, basisFn, params, priceMatrix, cfMatrix)
          recurseCF(step-1, params, priceMatrix, newCFMatrix, basisFn)
        }
      }
    }

  /**
   * Calculate the value of the option specified
   * by the parameters using the least squares method.
   *
   * @param params The lsm parameters.
   */
  val lsmOptionValue = (params: LsmParams) => {
    val priceMatrix = genPriceMatrix(params)
    if (DEBUG)
      println("priceMatrix:\n"+priceMatrix)

    val initCFMatrix = new Matrix(params.numPaths, params.numSteps)

    // normalize parameter x to prevent underflows
    val basisFn = (x: Double) => Array (
      1.0,
      exp(-x/(2.0*params.stock)),
      exp(-x/(2.0*params.stock))*(1.0-x/params.stock),
      exp(-x/(2.0*params.stock))*(1.0-2.0*x/params.stock+sqr(x/params.stock)/2.0)
    )

    val cfMatrix = recurseCF(params.numSteps, params, priceMatrix, initCFMatrix, basisFn)

    //todo create stopping matrix

    if (DEBUG)
      println(cfMatrix)
    optionValueStdErr(params, cfMatrix)
  }

  /**
   * First walk through example in the LSM paper
   *
   */
  def lsmOptionValueSimpleExample() {
    val startTime = System.nanoTime
    val params = LsmParams( true, 8, 3, 3, 1.0, 1.10, 0.06, 0.0 )
    println ("params = "+params)

    val priceMatrix = new Matrix(Array(
      Array(1.00, 1.09, 1.08, 1.34),
      Array(1.00, 1.16, 1.26, 1.54),
      Array(1.00, 1.22, 1.07, 1.03),
      Array(1.00, 0.93, 0.97, 0.92),
      Array(1.00, 1.11, 1.56, 1.52),
      Array(1.00, 0.76, 0.77, 0.90),
      Array(1.00, 0.92, 0.84, 1.01),
      Array(1.00, 0.88, 1.22, 1.34)
    ))
    if (DEBUG)
      println("priceMatrix:\n"+priceMatrix)

    val initCFMatrix = new Matrix(params.numPaths, params.numSteps)
    val basisFn = (x: Double) => Array ( 1.0, x, sqr(x) )
    val cfMatrix = recurseCF(params.numSteps, params, priceMatrix, initCFMatrix, basisFn)

    if (DEBUG)
      println(cfMatrix)
    val endTime = System.nanoTime

    val lsmOV = optionValueStdErr(params, cfMatrix)
    prettyPrint(lsmOV, endTime-startTime)
  }

  private def prettyPrint(a: Array[Double]) {
    var str: String = "["
    var i = 0
    while (i < a.length) {
      str += "% 6.2f".format(a(i))
      if (i < a.length-1)
        str += "\n "
      i += 1
    }
    str += " ]\n"
    println(str)
  }

  private def prettyPrint(lsmOV: Tuple2[Double, Double], time: Double) {
    println(" "+"% 6.4f".format(lsmOV._1)+"  ( "+"%.4f".format(lsmOV._2)+" ) [ "+"%.3f".format(time/1e9)+"sec ]")
  }

  /**
   * Utility method to time the option price calculation.
   *
   * @param params The lsm parameters.
   *
   */
  def simulate(params: LsmParams)  {
    println ("params = "+params)
    val startTime = System.nanoTime
    val lsmOV = lsmOptionValue(params)
    val endTime = System.nanoTime
    prettyPrint(lsmOV, endTime-startTime)
  }

  def main(args: Array[String]): Unit = {
    println("Starting... ")

    lsmOptionValueSimpleExample()

    val test_01 = LsmParams( true, 10000, 1, 50, 36.0, 40.0, 0.06, 0.20 )
    val test_02 = LsmParams( true, 10000, 2, 100, 36.0, 40.0, 0.06, 0.20 )
    val test_03 = LsmParams( true, 10000, 1, 50, 36.0, 40.0, 0.06, 0.40 )
    val test_04 = LsmParams( true, 10000, 2, 100, 36.0, 40.0, 0.06, 0.40 )
    simulate(test_01)
    simulate(test_02)
    simulate(test_03)
    simulate(test_04)

    // examples from the table on p16 of the LSM paper
    val example2_01 = LsmParams( true, 100000, 1, 50, 36.0, 40.0, 0.06, 0.20 )
    val example2_02 = LsmParams( true, 100000, 2, 100, 36.0, 40.0, 0.06, 0.20 )
    val example2_03 = LsmParams( true, 100000, 1, 50, 36.0, 40.0, 0.06, 0.40 )
    val example2_04 = LsmParams( true, 100000, 2, 100, 36.0, 40.0, 0.06, 0.40 )

    val example2_05 = LsmParams( true, 100000, 1, 50, 38.0, 40.0, 0.06, 0.20 )
    val example2_06 = LsmParams( true, 100000, 2, 100, 38.0, 40.0, 0.06, 0.20 )
    val example2_07 = LsmParams( true, 100000, 1, 50, 38.0, 40.0, 0.06, 0.40 )
    val example2_08 = LsmParams( true, 100000, 2, 100, 38.0, 40.0, 0.06, 0.40 )

    val example2_09 = LsmParams( true, 100000, 1, 50, 40.0, 40.0, 0.06, 0.20 )
    val example2_10 = LsmParams( true, 100000, 2, 100, 40.0, 40.0, 0.06, 0.20 )
    val example2_11 = LsmParams( true, 100000, 1, 50, 40.0, 40.0, 0.06, 0.40 )
    val example2_12 = LsmParams( true, 100000, 2, 100, 40.0, 40.0, 0.06, 0.40 )

    val example2_13 = LsmParams( true, 100000, 1, 50, 42.0, 40.0, 0.06, 0.20 )
    val example2_14 = LsmParams( true, 100000, 2, 100, 42.0, 40.0, 0.06, 0.20 )
    val example2_15 = LsmParams( true, 100000, 1, 50, 42.0, 40.0, 0.06, 0.40 )
    val example2_16 = LsmParams( true, 100000, 2, 100, 42.0, 40.0, 0.06, 0.40 )

    val example2_17 = LsmParams( true, 100000, 1, 50, 44.0, 40.0, 0.06, 0.20 )
    val example2_18 = LsmParams( true, 100000, 2, 100, 44.0, 40.0, 0.06, 0.20 )
    val example2_19 = LsmParams( true, 100000, 1, 50, 44.0, 40.0, 0.06, 0.40 )
    val example2_20 = LsmParams( true, 100000, 2, 100, 44.0, 40.0, 0.06, 0.40 )

    simulate(example2_01)
    simulate(example2_02)
    simulate(example2_03)
    simulate(example2_04)
    simulate(example2_05)
    simulate(example2_06)
    simulate(example2_07)
    simulate(example2_08)
    simulate(example2_09)
    simulate(example2_10)
    simulate(example2_11)
    simulate(example2_12)
    simulate(example2_13)
    simulate(example2_14)
    simulate(example2_15)
    simulate(example2_16)
    simulate(example2_17)
    simulate(example2_18)
    simulate(example2_19)
    simulate(example2_20)

    println("Complete")
  }
}
