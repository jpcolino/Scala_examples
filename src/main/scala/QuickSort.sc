/* Quicksort */

class Quicksort {

  def sort(a:Array[Int]): Array[Int] =
    if (a.length < 2) a
    else {
      val pivot = a(a.length / 2)
      sort (a filter (pivot>)) ++ (a filter (pivot == )) ++
        sort (a filter(pivot <))
    }

  def qsort[T <% Ordered[T]](list: List[T]): List[T] = {
    list match {
      case Nil => Nil
      case x::xs =>
        val (before, after) = xs partition (_ < x)
        qsort(before) ++ (x :: qsort(after))
    }
  }
}


val quicksort = new Quicksort
val a = List(5, 3, 2, 2, 1, 1, 9, 39, 219)
val b = Array(5, 3, 2, 2, 1, 1, 9, 39, 219)
quicksort.qsort(a).foreach(n => (print(n), print(" ")))
quicksort.sort(b).foreach(n => (print(n), print(" ")))

