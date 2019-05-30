package playground.imperative

import java.lang.Math.min

import playground.Sorter

object MergeSorter extends Sorter {

  override type A = Int

  override def sort(array: Array[A])(compare: (A, A) => Int): Unit = {
    topDownMergeSort(array, new Array[A](array.length), array.length)
  }


  // Array A[] has the items to sort; array B[] is a work array.
  def topDownMergeSort(A: Array[A], B: Array[A], n: Int): Unit = {
    copyArray(A, 0, n, B);           // duplicate array A[] into B[]
    topDownSplitMerge(B, 0, n, A);   // sort data from B[] into A[]
  }

  // Sort the given run of array A[] using array B[] as a source.
  // iBegin is inclusive; iEnd is exclusive (A[iEnd] is not in the set).
  def topDownSplitMerge(B: Array[A], iBegin: Int, iEnd: Int, A: Array[A]): Unit = {
    if(iEnd - iBegin < 2)                       // if run size == 1
      return                                    // consider it sorted
                                                // split the run longer than 1 item into halves
    val iMiddle = (iEnd + iBegin) / 2;          // iMiddle = mid point
    // recursively sort both runs from array A[] into B[]
    topDownSplitMerge(A, iBegin,  iMiddle, B)   // sort the left  run
    topDownSplitMerge(A, iMiddle,    iEnd, B)   // sort the right run
    // merge the resulting runs from array B[] into A[]
    topDownMerge(B, iBegin, iMiddle, iEnd, A)
  }

  //  Left source half is A[ iBegin:iMiddle-1].
  // Right source half is A[iMiddle:iEnd-1   ].
  // Result is            B[ iBegin:iEnd-1   ].
  def topDownMerge(A: Array[A], iBegin: Int, iMiddle: Int, iEnd: Int, B: Array[A]): Unit = {
    var i = iBegin
    var j = iMiddle

    // While there are elements in the left or right runs...
    for (k <- iBegin until iEnd) {
      // If left run head exists and is <= existing right run head.
      if (i < iMiddle && (j >= iEnd || A(i) <= A(j))) {
        B(k) = A(i)
        i += 1
      } else {
        B(k) = A(j)
        j += 1
      }
    }
  }

  def copyArray(A: Array[A], iBegin: Int, iEnd: Int, B: Array[A]): Unit = {
    for(k <- iBegin until iEnd) {
      B(k) = A(k)
    }
  }



  // array A[] has the items to sort; array B[] is a work array
  def bottomUpMergeSort(A: Array[A], B: Array[A], n: Int): Unit = {
    // Each 1-element run in A is already "sorted".
    // Make successively longer sorted runs of length 2, 4, 8, 16... until whole array is sorted.
    var width = 1
    while (width < n) {
      // Array A is full of runs of length width.
      var i = 0
      while (i < n) {
        // Merge two runs: A[i:i+width-1] and A[i+width:i+2*width-1] to B[]
        // or copy A[i:n-1] to B[] ( if(i+width >= n) )
        bottomUpMerge(A, i, min(i + width, n), min(i + 2 * width, n), B)
        i = i + 2 * width
      }
      // Now work array B is full of runs of length 2*width.
      // Copy array B to array A for next iteration.
      // A more efficient implementation would swap the roles of A and B.
      copyArray(B, A, n)
      // Now array A is full of runs of length 2*width.
      width *= 2
    }
  }

  //  Left run is A[iLeft :iRight-1].
  // Right run is A[iRight:iEnd-1  ].
  def bottomUpMerge(A: Array[A], iLeft: Int, iRight: Int, iEnd: Int, B: Array[A]): Unit = {
    var i = iLeft
    var j = iRight
    // While there are elements in the left or right runs...
    for (k <- iLeft until iEnd) {
      // If left run head exists and is <= existing right run head.
      if (i < iRight && (j >= iEnd || A(i) <= A(j))) {
        B(k) = A(i)
        i = i + 1
      } else {
        B(k) = A(j)
        j = j + 1
      }
    }
  }

  def copyArray(B: Array[A], A: Array[A], n: Int): Unit ={
    for(i <- 0 until n) {
      A(i) = B(i)
    }
  }


//  private def mergeSort(array: Array[A])(compare: (A, A) => Int): Array[A] = {
//
//    println("MERGE SORT TRY")
//
//    if (array.length <= 1) {
//      array
//    } else {
//      var left = new Array[A](0)
//      var right = new Array[A](0)
//
//      for (i <- array.indices) {
//        println(s"STEP: $i")
//        if (i < array.length / 2)
//          left = left :+ array(i)
//        else
//          right = right :+ array(i)
//      }
//
//      left = mergeSort(left)(compare)
//      right = mergeSort(right)(compare)
//
//      merge(left, right)(compare)
//    }
//  }

//  private def merge(left: Array[A], right: Array[A])(compare: (A, A) => Int): Array[A] = {
//    var result = new Array[A](0)
//
//    println(s"MERGE: ${left.foldLeft("")(_+_)} ---> <--- ${right.foldLeft("")(_+_)}")
//
//    var leftHolder = left
//    var rightHolder = right
//
//    while (leftHolder.nonEmpty && rightHolder.nonEmpty) {
//      if (compare(leftHolder.head, rightHolder.head) <= 0) {
//        result = result :+ leftHolder.head
//        leftHolder = leftHolder.tail
//      } else {
//        result = result :+ rightHolder.head
//        rightHolder = rightHolder.tail
//      }
//    }
//
//    result
//  }

}
