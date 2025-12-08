package lists

import lists.EList.{flatten, partition}
import scala.annotation.tailrec
import scala.math.Ordering
import scala.util.Random

/**
 * Same as ListProblems, but using Scala 3's enum for sum type instead of a sealed abstract class.
 * The 'E' in EList stands for enum. I have also added leftFold and rightFold and implemented some of the
 * problems using folds.
*/

enum EList[+T] {
  case Empty()
  case ::(head: T, tail: EList[T])

  // Cons operator to prepend an element to the list
  def ::[S >: T](elem: S): EList[S] = new ::(elem, this)

  def headOption: Option[T] = this match {
    case Empty() => None
    case head :: _ => Some(head)
  }

  //--------------------------------------------------------------------------------------
  override def toString: String = this match {
    case Empty()      => "[]"
    case head :: tail =>
      @tailrec
      def toStringHelper(remaining: EList[T], acc: String): String = remaining match {
        case Empty()      => acc
        case head :: tail => toStringHelper(tail, s"$acc, $head")
      }
      "[" + toStringHelper(tail, s"$head") + "]"
  }

  //--------------------------------------------------------------------------------------
  // retrieve the element at index
  def apply(index: Int): T = {
    @tailrec
    def applyHelper(remaining: EList[T], curIndex: Int): T = remaining match {
      case Empty()      => throw new NoSuchElementException()
      case head :: tail =>
        if curIndex == index then head  // return the element at the index
        else applyHelper(tail, curIndex + 1)
    }
    if index < 0 then
      throw new NoSuchElementException()
    else
      applyHelper(this, 0)
  }

  //--------------------------------------------------------------------------------------
  // LENGTH
  // Naturally recursive version of length. Not stack-safe
  def lengthSR: Int = this match {
    case Empty() => 0
    case _ :: tail => 1 + tail.lengthSR
  }

  // Implementing length with the foldLeft function
  def lengthFold: Int = foldLeft(0)((_, acc) => 1 + acc)

  def length: Int = {
    @tailrec
    def lengthHelper(remaining: EList[T], curLength: Int): Int = remaining match {
      case Empty()    => curLength
      case _ :: tail  => lengthHelper(tail, 1 + curLength)
    }
    lengthHelper(this, 0)
  }

  //--------------------------------------------------------------------------------------
  def reverse: EList[T] = {
    @tailrec
    def reverseHelper(remaining: EList[T], acc: EList[T]): EList[T] = remaining match {
      case Empty()      => acc
      case head :: tail => reverseHelper(tail, head :: acc)
    }
    reverseHelper(this, Empty())
  }

  //--------------------------------------------------------------------------------------
  def ++[S >: T](other: EList[S]): EList[S] = this match {
    case Empty() => other
    case head :: tail =>
      @tailrec // implementation identical to reverseHelper
      def concatHelper(remaining: EList[T], acc: EList[S]): EList[S] = remaining match {
        case Empty()      => acc
        case head :: tail => concatHelper(tail, head :: acc)
      }
      concatHelper(this.reverse, other)
  }

  //--------------------------------------------------------------------------------------
  /**
   * Removes the element from the given index. If the index is out of bounds, return the
   * list unaltered.
   */
  def removeAt[S >: T](index: Int): EList[S] = this match {
    case Empty() => Empty()
    case _ =>
      @tailrec
      def removeAtHelper(remaining: EList[S], curIndex: Int, traversed: EList[S]): EList[S] =
        remaining match {
          case Empty() => this // index is beyond the length of the list
          case head :: tail =>
            if curIndex == index then // remove the current element - the head
              traversed.reverse ++ tail
            else // move on to the next element in the list
              removeAtHelper(tail, curIndex + 1, head :: traversed)
        }

      if index < 0 then this
      else removeAtHelper(this, 0, Empty())
  }

  //--------------------------------------------------------------------------------------
  def map[U](f: T => U): EList[U] = this match {
    case Empty() => Empty()
    case _ =>
      @tailrec
      def mapHelper(remaining: EList[T], acc: EList[U]): EList[U] = remaining match {
        case Empty()      => acc.reverse
        case head :: tail => mapHelper(tail, f(head) :: acc)
      }
      mapHelper(this, Empty())
  }

  //--------------------------------------------------------------------------------------
  def filter(predicate: T => Boolean): EList[T] = this match {
    case Empty()  => Empty()
    case _        =>
      @tailrec
      def filterHelper(remaining: EList[T], acc: EList[T]): EList[T] = remaining match {
        case Empty()      => acc.reverse
        case head :: tail =>
          val newAcc = if predicate(head) then head :: acc else acc
          filterHelper(tail, newAcc)
      }
      filterHelper(this, Empty())
  }

  //--------------------------------------------------------------------------------------
  def flatMap[U](f: T => EList[U]): EList[U] = flatten(this.map(f))

  //--------------------------------------------------------------------------------------  
  // Tail recursive, but has poor performance, as it does ++ at each step. Complexity is O(N^2)
  def flatMapInefficient[U](f: T => EList[U]): EList[U] = this match {
    case Empty()  => Empty()
    case _        =>
      @tailrec
      def flatMapHelper(remaining: EList[T], acc: EList[U]): EList[U] = remaining match {
        case Empty()      => acc
        case head :: tail =>
          flatMapHelper(tail, acc ++ f(head))
      }
      flatMapHelper(this, Empty())  
  }
  
  //--------------------------------------------------------------------------------------
  // run-length encoding - medium difficulty problem
  /* [7, 7, 4, 3, 3].rle
      = rleHelper([7, 4, 3, 3], 7, 1, [])
      = rleHelper([4, 3, 3],    7, 2, [])       // head = 7, prev = 7, count = 0
      = rleHelper([3, 3],       4, 1, [(7,2)])  // head = 4
      = rleHelper([3],          3, 1, [(4, 1), (7, 2)])
   */
  def rle: EList[(T, Int)] = this match {
    case Empty() => Empty()
    case ::(head, tail) =>  // [7, 7, 4, 3, 3]
      @tailrec
      def rleHelper(
       remaining: EList[T], prev: T, count: Int, 
       acc: EList[(T, Int)]): EList[(T, Int)] = remaining match {
        case Empty()        => (prev, count) :: acc
        case head :: tail =>
          if head == prev then // same element as previous, so increment the count and move on
            rleHelper(tail, head, count + 1, acc)
          else  // new list element
            rleHelper(tail, head, 1, (prev, count) :: acc)
        }
      rleHelper(tail, head, 1, Empty()).reverse  
  }
  
  //--------------------------------------------------------------------------------------
  // Duplicate each list element 'count' number of times
  // [7, 8, 9].duplicateEach(2) == [7, 7, 8, 8, 9, 9]
  // Complexity is O(N * k), where k = count
  
  // My solution is simpler than the instructor's. He used an additional parameter for the head in
  // the helper function. He also used 4 condition checks rather than 3, but the results of 2 of those
  // 4 conditions was the same code. So clearly 3 conditions would have sufficed.
  def duplicateEach(count: Int): EList[T] = this match {
    case Empty()  => Empty()
    case _        =>
      @tailrec
      def duplicateEachHelper(remaining: EList[T], times: Int, acc: EList[T]): EList[T] = remaining match {
        case Empty() => acc.reverse
        case head :: tail =>
          if times == count then  // move on to the next element in the list
            duplicateEachHelper(tail, 0, acc)
          else
            duplicateEachHelper(remaining, times + 1, head :: acc)
      }
      if count <= 0 then Empty() 
      else if count == 1 then this 
      else duplicateEachHelper(this, 0, Empty())
  }

  //--------------------------------------------------------------------------------------
  /*
    Rotate left k places
    Example: [1,2,3,4,5,6,7,8].rotate(3) == [4,5,6,7,8,1,2,3]
    = rotateHelper([1,2,3,4,5,6,7,8], [], 0)
    = rotateHelper([2,3,4,5,6,7,8], [1], 1)
    = rotateHelper([3,4,5,6,7,8], [2,1], 2)
    = rotateHelper([4,5,6,7,8], [3,2,1], 3)
    = [4,5,6,7,8] ++ [3,2,1].reverse
    = [4,5,6,7,8,1,2,3]
  
    Complexity: O(max(N, k))
   */
  def rotate(k: Int): EList[T] = this match {
    case Empty() => Empty()
    case _ =>
      @tailrec
      def rotateHelper(remaining: EList[T], rotationsLeft: Int, right: EList[T]): EList[T] =
        (remaining, rotationsLeft) match {
          case (Empty(), 0)       => this // k was an exact multiple of the length of the list
          case (Empty(), n)       => rotateHelper(this, n, Empty()) // rotationsLeft == k mod (length of list)
          case (_, 0)             => remaining ++ right.reverse
          case (head :: tail, n)  => rotateHelper(tail, n - 1, head :: right)
        }
      if k <= 0 then this
      else rotateHelper(this, k, Empty())  
  }
  
  //--------------------------------------------------------------------------------------
  // random sample. Return a new list containing k random samples from the original list.
  // k can be larger than the length of the list
  // Complexity is O(N * K)
  def randomSample(k: Int): EList[T] = this match {
    case Empty() => Empty()
    case _ =>
      val maxIndex = this.length
      val random = new Random(System.currentTimeMillis())
      
      @tailrec
      def sampleHelper(doneCount: Int, acc: EList[T]): EList[T] =
        if doneCount == k then acc
        else {
          val index = random nextInt maxIndex
          sampleHelper(doneCount + 1, this(index) :: acc)
        }

      if k <= 0 then Empty()
      else sampleHelper(0, Empty())
  }
  
  // Elegant version using functional composition
  def randomSampleElegant(k: Int): EList[T] =
    if k <= 0 then Empty()
    else {
      val maxIndex = this.length
      val random = new Random(System.currentTimeMillis())
      EList.from(1 to k)
        .map(_ => this(random nextInt maxIndex))
    }
    
  //--------------------------------------------------------------------------------------
  /**
   * Hard problems
   */
  
  // insertion sort
  def insertionSort[S >: T](ordering: Ordering[S]): EList[S] = {
    /*
      [1,2,5,8].sorted = insertionSort([1,2,5,8], [])
        = insertionSort([1,2,5,8], [])
        = insertionSort([2,5,8], [1])
        = insertionSort([5,8], [1,2])
        = insertionSort([8], [1,2,5])
        = insertionSort([1,2,5,8], [])
     */
    @tailrec
    def insertionSortHelper(remaining: EList[T], acc: EList[S]): EList[S] = remaining match {
      case Empty() => acc
      case head :: tail =>
        /*
          insertSorted(4, [1,2,3,5], []) =
          insertSorted(4, [2,3,5], [1] =
          insertSorted(4, [3,5], [2,1]) =
          insertSorted(4, [5], [3,2,1]) =
          [3,2,1].reverse ++ (4 :: [5]) =
          [1,2,3,4,5]
         */
        @tailrec
        def insertSorted(elem: T, sortedList: EList[S], traversed: EList[S]): EList[S] = sortedList match {
          // elem has to be appended to the end of the traversed list
          case Empty() => traversed.reverse ++ (elem :: sortedList)
          case head :: tail =>
            if ordering.gt(elem, head) then
              // elem > head, so move on to the next element in sorted list
              insertSorted(elem, tail, head :: traversed)
            else  // add element at the appropriate location
              traversed.reverse ++ (elem :: sortedList)
        }

        // insert head into appropriate location in acc
        val newAcc = insertSorted(head, acc, Empty())
        insertionSortHelper(tail, newAcc)
    }
    insertionSortHelper(this, Empty())
  }

  //--------------------------------------------------------------------------------------
  def mergeSort[S >: T](ordering: Ordering[S]): EList[S] = this match {
    case Empty() => Empty()
    case head :: Empty() => head :: Empty() // single element
    case nonEmptyList =>

      @tailrec  // merge two sorted lists into a single sorted list
      def merge(leftRem: EList[S], rightRem: EList[S], acc: EList[S]): EList[S] =
        (leftRem, rightRem) match {
          case (Empty(), Empty()) => acc.reverse
          case (Empty(), right) => acc.reverse ++ right
          case (left, Empty()) => acc.reverse ++ left
          case (x :: xs, y :: ys) =>
            ordering.compare(x, y) match {
              case n if n < 0 => merge(xs, y :: ys, x :: acc)
              case 0          => merge(xs, ys, x :: y :: acc)
              case _          => merge(x :: xs, ys, y :: acc)
            }
        }

      /*
        [3,1,2,5,4] = [[3],[1],[2],[5],[4]]
        msh([[3],[1],[2],[5],[4]], [])      // case E
        = msh([[2],[5],[4]], [[1,3]])       // case E
        = msh([[4]], [[2,5], [1,3]])        // case D
        = msh([], [[4], [2,5], [1,3]])      // case C: swap bigLists and smallLists, since smallList is empty
        = msh([[4], [2,5], [1,3]], [])      // case E
        = msh([[1,3]], [[2,4,5]])           // case D without swap
        = msh([], [[1,3], [2,4,5]])         // case C: time to swap
        = msh([[1,3], [2,4,5]], [])         // case E
        = msh([], [[1,2,3,4,5]])            // case B
        = [1,2,3,4,5]

        Complexity: O(n * log n)
        Equation: complexity(n) = 2 * complexity(n/2) + n
        In the equation, the "+ n" is the complexity of the merge.
       */
      @tailrec
      def mergeSortHelper(smallLists: EList[EList[S]], bigLists: EList[EList[S]]): EList[S] =
        smallLists match {
          case Empty() => // If bigLists contains just one list then return it else swap lists
            bigLists match {
              case Empty()          => Empty()  // case A
              case head :: Empty()  => head     // case B: return the sole nested list
              case _                => mergeSortHelper(bigLists, Empty()) // case C: swap big and small lists
            }

          // case D: smallList has just one element, so prepend it to bigLists and swap bigList and smallList
          case head :: Empty() => mergeSortHelper(head :: bigLists, Empty())

          // case E: smallList has at least 2 elements
          case h1 :: h2 :: tail =>
            val newBigLists = merge(h1, h2, Empty()) :: bigLists
            mergeSortHelper(tail, newBigLists)
        }

      mergeSortHelper(this.wrap, Empty())
  }

  //=======================================================================================
  // QUICK SORT

  // Stack recursive version
  def quickSortSR[S >: T](ordering: Ordering[S]): EList[S] = this match {
    case Empty() => Empty()
    case pivot :: tail =>
      val (lesser, greater) = partition(tail, pivot, ordering)
      lesser.quickSortSR(ordering) ++ (pivot :: Empty()) ++ greater.quickSortSR(ordering)
  }

  //--------------------------------------------------------------------------------------
  // Tail recursive version
  def quickSort[S >: T](ordering: Ordering[S]): EList[S] = this match {
    case Empty() => Empty()
    case list =>
      /*
        [4,3,2,7,5,0,1].quickSort
        = qsh([[4,3,2,7,5,0,1]], [])
        = qsh([[3,2,0,1], [4], [7,5]], [])
        = qsh([[2,0,1], [3], [], [4], [7,5]], [])
        = qsh([[0,1], [2], [3], [], [4], [7,5]], [])
        = qsh([[0], [1], [2], [3], [], [4], [7,5]], [])
        = qsh([[7,5]], [[4], [3], [2], [1], [0]])    // takes several steps to get to this point
        = qsh([[5], [7]], [[4], [3], [2], [1], [0]])
        = qsh([], [[7], [5], [4], [3], [2], [1], [0]])
        = [0,1,2,3,4,5,7]
       */
      @tailrec
      def quickSortHelper(remaining: EList[EList[S]], acc: EList[EList[S]]): EList[S] = remaining match {
        case Empty() => acc match {
          case Empty()  => Empty()    // case 1
          case nested   => nested.flatMap(identity).reverse // case 2
        }
        case xs :: xss =>
          xs match {
            // nested list is empty, so remove it. Example: [[], [5], [7], [8,9]]
            case Empty() => quickSortHelper(xss, acc) // case 3

            // nested list has a single element
            // example: [[5], [7], [8,9]]. Here xs = [5], xss = [[7], [8,9]], x = 7
            case x :: Empty() => quickSortHelper(xss, xs :: acc)  // case 4

            // nested list has multiple elements. example: [[3,2,0,1], [4], [7,5]]
            case pivot :: ys => // pivot = 3 in this example - case 5
              val (smaller, greater) = partition(ys, pivot, ordering)
              val newRemaining = smaller :: (pivot :: Empty()) :: greater :: xss
              quickSortHelper(newRemaining, acc)
        }
      }
      val listOfList = list :: Empty()
      quickSortHelper(listOfList, Empty())
  }

  //--------------------------------------------------------------------------------------
  // Tail recursive version optimized for lists containing duplicates
  def quickSortOptimized[S >: T](ordering: Ordering[S]): EList[S] = this match {
    case Empty() => Empty()
    case list =>
      /*
       Splits list into two based on the pivot but excludes the pivot from both lists.
       Instead also returns the number of times the pivot occurs in the list.
       Example: partition([4,3,5,2,3,0,1], 3, [], []) = (2, [2,0,1], [4,5])
      */
      @tailrec
      def partitionThreeWay(list: EList[S], pivot: S, pivotCount: Int,
                    smaller: EList[S], larger: EList[S]): (Int, EList[S], EList[S]) = list match {
        case Empty() => (pivotCount, smaller, larger)
        case head :: tail =>
          ordering.compare(head, pivot) match {
            case n if n < 0 =>  partitionThreeWay(tail, pivot, pivotCount, head :: smaller, larger)
            case n if n == 0 => partitionThreeWay(tail, pivot, pivotCount + 1, smaller, larger) // increase pivot count
            case _ =>           partitionThreeWay(tail, pivot, pivotCount, smaller, head :: larger)
          }
      }

      /*
        Prepends a singleton list containing 'pivot', a 'pivotCount' number of times to the given list
        Example: prependPivots([[7,5]], 10, 3) = [[10], [10], [10], [7,5]]
       */
      @tailrec
      def prependPivots(list: EList[EList[S]], pivot: S, pivotCount: Int): EList[EList[S]] = pivotCount match {
        case n if n <= 0 => list
        case _ =>
          val newList = (pivot :: Empty()) :: list
          prependPivots(newList, pivot, pivotCount - 1)
      }

      @tailrec
      def quickSortHelper(remaining: EList[EList[S]], acc: EList[EList[S]]): EList[S] = remaining match {
        case Empty() => acc match {
          case Empty()  => Empty()                            // case 1
          case nested   => nested.flatMap(identity).reverse   // case 2. flatten the list and reverse it
        }
        case headList :: tailList =>
          headList match {
            // nested list is empty, so remove it. Example: [[], [5], [7], [8,9]]
            case Empty() => quickSortHelper(tailList, acc) // case 3

            // nested list has a single element
            // example: [[5], [7], [8,9]]. Here headList = [5], tailList = [[7], [8,9]], x = 7
            case x :: Empty() => quickSortHelper(tailList, headList :: acc) // case 4

            // nested list has multiple elements. example: [[3,2,0,1], [4], [7,5]]
            case pivot :: tail => // pivot = 3 in this example - case 5
              val (pivotCount, smaller, greater) = partitionThreeWay(tail, pivot, 1, Empty(), Empty())
              val newRemaining = smaller :: prependPivots(greater :: tailList, pivot, pivotCount)
              quickSortHelper(newRemaining, acc)
          }
      }

      val listOfList = list :: Empty()
      quickSortHelper(listOfList, Empty())
  }

  //--------------------------------------------------------------------------------------
  // Wraps each element into an EList
  def wrapSR: EList[EList[T]] = this match {
    case Empty() => Empty()
    case x :: xs => (x :: Empty()) :: xs.wrapSR
  }

  def wrap: EList[EList[T]] = this map (_ :: Empty())

  //--------------------------------------------------------------------------------------
  // FOLDS
  @tailrec
  final def foldLeft[B](empty: B)(f: (T, B) => B): B = this match {
    case Empty()      => empty
    case head :: tail => tail.foldLeft(f(head, empty))(f)
  }

  // types:
  // head:      T
  // f:         B
  // foldRight: B
  def foldRight[B](empty: B)(f: (T, B) => B): B = this match {
    case Empty()      => empty
    case head :: tail => f(head, tail.foldRight(empty)(f))
  }

} // EList

//=============================================================================================================
// companion object
object EList {
  def from[T](iterable: Iterable[T]): EList[T] = {
    @tailrec
    def fromHelper(remaining: Iterable[T], acc: EList[T]): EList[T] =
      if remaining.isEmpty then acc.reverse
      else fromHelper(remaining.tail, remaining.head :: acc)
    
    fromHelper(iterable, Empty())
  }

  //--------------------------------------------------------------------------------------
  // flattens a nested list
  // [[1,2,3],[4,5],[6],[7,8,9]]
  def flatten[T](nested: EList[EList[T]]): EList[T] = nested match {
    case Empty() => Empty()
    case _ =>
      @tailrec
      def flattenHelper(remaining: EList[EList[T]], acc: EList[T]): EList[T] = remaining match {
        case Empty() => acc.reverse // e.g. [1,2,3]
        case head :: tail => // head: EList[T] e.g. [1,2,3]
          @tailrec // add elements from the nested list to the accumulator
          def helper2(pending: EList[T], accum: EList[T]): EList[T] = pending match {
            case Empty() => accum
            case head :: tail => helper2(tail, head :: accum)
          }
          val newAcc = helper2(head, acc)
          flattenHelper(tail, newAcc)
      }
      flattenHelper(nested, Empty())
  }

  //--------------------------------------------------------------------------------------
  // Moved it as a nested method for mergeSort algorithm later. Placed it here at the beginning just for testing.
  @tailrec
  def mergeSortedLists[T](ordering: Ordering[T], leftRem: EList[T], rightRem: EList[T], acc: EList[T]): EList[T] =
    (leftRem, rightRem) match {
      case (Empty(), Empty()) => acc.reverse
      case (Empty(), r)       => acc.reverse ++ r
      case (l, Empty())       => acc.reverse ++ l
      case (x :: xs, y :: ys) =>
        ordering.compare(x, y) match {
          case n if n < 0 => mergeSortedLists(ordering, xs, y :: ys, x :: acc)
          case 0          => mergeSortedLists(ordering, xs, ys, x :: y :: acc)
          case _          => mergeSortedLists(ordering, x :: xs, ys, y :: acc)
        }
    }

  //--------------------------------------------------------------------------------------
  /*
    Splits list into a two-way partition based on the pivot.
    partition([4,3,5,2,0,1], 3, [], []) = ([1,0,2,3], [5,4])
  */
  def partition[T](list: EList[T], pivot: T, ordering: Ordering[T]): (EList[T], EList[T]) = {
    @tailrec
    def partitionHelper(list: EList[T], smaller: EList[T], larger: EList[T]): (EList[T], EList[T]) = list match {
      case Empty() => (smaller, larger)
      case head :: tail =>
        if ordering.lteq(head, pivot) then
          partitionHelper(tail, head :: smaller, larger)
        else
          partitionHelper(tail, smaller, head :: larger)
    }
    partitionHelper(list, Empty(), Empty())
  }
}

//---------------------------------------------------------------------------------------------------------------------
object ListProblemsTest extends App {

  import EList.*
  val empty = Empty()
  val aList = 7 :: 8 :: 9 :: 10 :: empty
  val bList = 3 :: 4 :: 5 :: empty

  //------------------------------------------------------
  def testEasyProblems() = {
    // test toString
    println(aList) // [7, 8, 9, 10]
    println(Empty()) // []

    // test from
    println(EList.from(15 to 20)) // [15, 16, 17, 18, 19, 20]
    
    // test get k-th element
    println(aList(2)) // 9
    println(aList(0)) // 7
    // println(aList(5)) // NoSuchElement exception

    // test length
    println("\nTest length")
    println(bList.length)     // 3
    println(bList.lengthSR)   // 3
    println(bList.lengthFold) // 3

    println(empty.length)     // 0
    println(empty.lengthSR)   // 0
    println(empty.lengthFold) // 0

    // test reverse
    println("\nTest reverse")
    println(aList.reverse) // [10, 9, 8, 7]
    println(empty.reverse) // []

    // test concat
    println(aList ++ bList) // [7, 8, 9, 10, 3, 4, 5]
    println(empty ++ aList) // [7, 8, 9, 10]
    println(bList ++ empty) // [3, 4, 5]

    // test removeAt
    println(aList.removeAt(2)) // [7, 8, 10]
    println(aList.removeAt(0)) // [8, 9, 10]
    println(aList.removeAt(-1)) // [7, 8, 9, 10]
    println(aList.removeAt(15)) // [7, 8, 9, 10]

    // test map
    println(aList.map(_ % 2 == 0)) // [false, true, false, true]

    // test filter
    println((aList ++ bList).filter(_ % 2 != 0)) // [7, 9, 3, 5]

    // test flatMap
    println(aList.flatMapInefficient(x => x :: -x :: 2 * x :: Empty())) // [7, -7, 14, 8, -8, 16, 9, -9, 18, 10, -10, 20]
    println(aList.flatMap(x => x :: -x :: 2 * x :: Empty()))    // [7, -7, 14, 8, -8, 16, 9, -9, 18, 10, -10, 20]
  }

  //------------------------------------------------------
  val repList = 1 :: 1 :: 1 :: 2 :: 5 :: 5 :: Empty()
  val single  = 100 :: Empty()
  val double  = 7 :: 7 :: Empty()
  
  val nestedList = 
    (1 :: 2 :: 3 :: Empty()) 
      :: (4 :: 5 :: Empty()) 
      :: (6 :: Empty()) 
      :: (7 :: 8 :: Empty()) 
      :: Empty()

  val aLargeList = EList.from(1 to 10000)
  val listToSort = aLargeList.randomSample(10)

  //------------------------------------------------------
  def testMediumDifficultyProblems() = {
    
    // test run-length encoding
    println(aList.rle) // [(7,1), (8,1), (9,1), (10,1)]
    println(repList.rle) // [(1,3), (2,1), (5,2)]
    println(single.rle) // [(100, 1)]
    println(double.rle) // [(7, 2)]
    
    // test duplicateEach
    println(bList.duplicateEach(3)) // [3, 3, 3, 4, 4, 4, 5, 5, 5]
    println(aList.duplicateEach(2)) // [7, 7, 8, 8, 9, 9, 10, 10]
    
    // test rotate (rotate left)
    // aList = [7, 8, 9, 10]
    println(aList.rotate(3)) // [10, 7, 8, 9]
    println(aList.rotate(6)) // [9, 10, 7, 8]   // number of rotate places exceeds length of list
    println(aList.rotate(-5)) // [7, 8, 9, 10]  // invalid number of rotation places
    
    // test random sample
    println(aList.randomSample(10))
    println(bList.randomSampleElegant(10))
    
    // nested lists and flatten
    println(nestedList)                 // [[1, 2, 3], [4, 5], [6], [7, 8]]
    println(EList.flatten(nestedList))  // [1, 2, 3, 4, 5, 6, 7, 8]
    
    // test flatMap
    val startTime = System.currentTimeMillis()
    val aLargerList = aLargeList flatMap { x => x :: -x :: Empty() }
    val endTime = System.currentTimeMillis()
    // println(aLargerList)
    println(s"Time taken for flatMap of 10,000-element list = ${endTime - startTime} ms")
  }

  //------------------------------------------------------

  val jumbledList = 3 :: 2 :: 1 :: 6 :: 5 :: 4 :: 0 :: Empty()
  val ordering = Ordering.fromLessThan[Int](_ < _)
  println(jumbledList.insertionSort(ordering))
  println(jumbledList.mergeSort(ordering)) // raises an exception!

  val list2 = 9 :: 8 :: 7 :: 6 :: Empty()
  println(list2.insertionSort((x, y) => x - y)) // [6, 7, 8, 9]

  val odds  = 1 :: 3 :: 5 :: 7 :: Empty()
  val evens = 2 :: 4 :: 6 :: Empty()
  val before = 0 :: 1 :: Empty()
  val after = 8 :: 9 :: Empty()

  val singletonList = 5 :: Empty()

  //---------------------------------------------------------------------
  def testDifficultProblems(): Unit = {

    // test merge sorted lists
    println(EList.mergeSortedLists(ordering, odds, evens, Empty()))   // [1, 2, 3, 4, 5, 6, 7]
    println(EList.mergeSortedLists(ordering, before, evens, Empty())) // [0, 1, 2, 4, 6]
    println(EList.mergeSortedLists(ordering, evens, after, Empty()))  // [2, 4, 6, 8, 9]

    println(EList.mergeSortedLists(ordering, Empty(), evens, Empty()))  // [2, 4, 6]
    println(EList.mergeSortedLists(ordering, odds, Empty(), Empty()))   // [1, 3, 5, 7]

    // test wrapping each element of a list into a list
    println(evens.wrapSR) // [[2], [4], [6]]
    println(after.wrap)   // [[8], [9]]

    // test merge sort
    println(jumbledList.mergeSort(ordering))  // [0, 1, 2, 3, 4, 5, 6]
    println(Empty().mergeSort(ordering))      // []
    println(listToSort.mergeSort(ordering))   // random list should appear sorted

    // edge case for merge sort: singleton list
    println(singletonList.mergeSort(ordering))  // [5]

    // ------------- Quick Sort --------------------
    // Stack-recursive, tail-recursive and optimized versions

    println(jumbledList.quickSortSR(ordering))        // [0, 1, 2, 3, 4, 5, 6]
    println(jumbledList.quickSort(ordering))          // [0, 1, 2, 3, 4, 5, 6]
    println(jumbledList.quickSortOptimized(ordering)) // [0, 1, 2, 3, 4, 5, 6]

    println(listToSort.quickSortSR(ordering))
    println(listToSort.quickSort(ordering))
    println(listToSort.quickSortOptimized(ordering))

    println(singletonList.quickSortSR(ordering))          // [5]
    println(singletonList.quickSort(ordering))            // [5]
    println(singletonList.quickSortOptimized(ordering))   // [5]

    val doubleList = 2 :: 1 :: Empty()
    // test list partitioning
    val (small, large) = partition(doubleList, 2, ordering)
    println(s"smaller = $small, larger = $large") // smaller = [1,2], larger = []

    // List with 2 elements
    println(doubleList quickSortSR ordering) // [1, 2]
    println(doubleList quickSort ordering) // [1, 2]
    println(doubleList quickSortOptimized ordering) // [1, 2]

    // List with duplicates
    val listWithDuplicates = 3 :: 1 :: 3 :: 2 :: 1 :: Empty()
    println(listWithDuplicates quickSortSR ordering)          // [1, 1, 2, 3, 3]
    println(listWithDuplicates quickSort ordering)            // [1, 1, 2, 3, 3]
    println(listWithDuplicates quickSortOptimized ordering)   // [1, 1, 2, 3, 3]

    // Empty list
    println(Empty() quickSortSR ordering)         // []
    println(Empty() quickSort ordering)           // []
    println(Empty() quickSortOptimized ordering)  // []
  }

  //------------------------------------------------------

  testEasyProblems()
  // testMediumDifficultyProblems()
  // testDifficultProblems()
}