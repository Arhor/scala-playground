package playground

trait Sorter {
  type A
  def sort(array: Array[A])(compare: (A, A) => Int): Unit
}
