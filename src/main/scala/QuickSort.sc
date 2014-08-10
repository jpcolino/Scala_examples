/* 3 different implementation of Quicksort
* ----------------------------------------------------------
*
* Quicksort is a divide and conquer algorithm.
*
* Quicksort first divides a large array into two smaller sub-array:
* the low elements and the high elements.
*
* Quicksort can then recursively sort the sub-arrays.
*
* The steps are:
*
* 1. Pick an element, called a pivot, from the array.
*
* 2. Reorder the array so that all elements with values less than
* the pivot come before the pivot,while all elements with values
* greater than the pivot come after it (equal values can go either way).
* After this partitioning, the pivot is in its final position.
* This is called the partition operation.
*
* 3. Recursively apply the above steps to the sub-array of elements
* with smaller values and separately to the sub-array of elements with greater values.
*
* */
import scala.collection.mutable.ArraySeq
class Quicksort {

  // First implementation: the smartest
  // ---------------------------------------------------
  def qsort1(a:Array[Int]): Array[Int] =
    if (a.length < 2) a
    else {
      val pivot = a(a.length / 2)
      qsort1 (a filter (pivot>)) ++ (a filter (pivot == )) ++
       qsort1 (a filter(pivot <))
    }
  // Second implementation: the obfuscated
  // ---------------------------------------------------
  def qsort2[T <% Ordered[T]](list: List[T]): List[T] = {
    list match {
      case Nil => Nil
      case x::xs =>
        val (before, after) = xs partition (_ < x)
        qsort2(before) ++ (x :: qsort2(after))
    }
  }
  // Third implementation: the dumbest
  // ---------------------------------------------------

  def findPivot(vals: ArraySeq[Int], lo: Int, hi: Int): Int = {
    return findMedian(vals, lo, (lo+hi)/2, hi)
  }

  def findMedian(vals: ArraySeq[Int], lo: Int, mid: Int, hi: Int): Int = {
    val loVal = vals(lo);
    val midVal = vals(mid);
    val hiVal = vals(hi);
    if (loVal > midVal) {
      if (midVal > hiVal) {
        return mid
      } else if (loVal > hiVal) {
        return hi
      } else {
        return lo
      }
    } else {
      if (loVal > hiVal) {
        return lo
      } else if (midVal > hiVal) {
        return hi
      } else {
        return mid
      }
    }
  }

    def partition(vals: ArraySeq[Int], left: Int, right: Int, pivot: Int): Int = {
      var leftVar = left
      var rightVar = right
      while (leftVar <= rightVar) {
        while (vals(leftVar) < pivot) {
          leftVar = leftVar + 1
        }
        while ((rightVar >= leftVar) && (vals(rightVar) >= pivot)) {
          rightVar = rightVar - 1
        }
        if (rightVar > leftVar) {
          swap(vals, leftVar, rightVar)
        }
      }
      return leftVar
    }

    def swap(vals: ArraySeq[Int], i: Int, j: Int) {
      val temp = vals(i)
      vals(i) = vals(j)
      vals(j) = temp
    }

    def qsort3(vals: ArraySeq[Int], lo: Int, hi: Int) {
      var pivotIdx = findPivot(vals, lo, hi)
      swap(vals, pivotIdx, hi)
      val mid = partition(vals, lo, hi - 1, vals(hi))
      swap(vals, mid, hi)
      if ((mid - lo) > 1) qsort3(vals, lo, mid - 1)
      if ((hi - mid) > 1) qsort3(vals, mid + 1, hi)
    }

}


val quicksort = new Quicksort
val a = Array(5, 3, 2, 2, 1, 1, 9, 39, 219)
val b = List(5, 3, 2, 2, 1, 1, 9, 39, 219)
val c = ArraySeq(5, 3, 2, 2, 1, 1, 9, 39, 219)


quicksort.qsort1(a).foreach(n => (print(n), print(" ")))
quicksort.qsort2(b).foreach(n => (print(n), print(" ")))
quicksort.qsort3(c,1,219)
