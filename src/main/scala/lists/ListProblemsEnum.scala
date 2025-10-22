package lists

import scala.annotation.tailrec

/**
 * Same as ListProblems, but using Scala 3's enum for sum type instead of a sealed abstract class.
 * The 'E' in EList stands for enum. I have also added leftFold and rightFold and implemented many of the
 * problems using folds.
*/

enum EList[+T] {
  case Empty()
  case ::(head: T, tail: EList[T])

  def headOption: Option[T] = this match {
    case Empty() => None
    case ::(head, _) => Some(head)
  }

  // Cons operator to prepend an element to the list
  def ::[S >: T](elem: S): EList[S] = new ::(elem, this)

  //--------------------------------------------------------------------------------------
  override def toString: String = this match {
    case Empty() => "[]"
    case ::(head, tail) =>
      @tailrec
      def toStringHelper(remaining: EList[T], acc: String): String = remaining match {
        case Empty()        => acc
        case ::(head, tail) => toStringHelper(tail, s"$acc, $head")
      }
      "[" + toStringHelper(tail, s"$head") + "]"
  }

  //--------------------------------------------------------------------------------------
  // retrieve the element at index
  def apply(index: Int): T = {
    @tailrec
    def applyHelper(remaining: EList[T], curIndex: Int): T = remaining match {
      case Empty()        => throw new NoSuchElementException()
      case ::(head, tail) =>
        if curIndex == index then head  // return the element at the index
        else applyHelper(tail, curIndex + 1)
    }
    if index < 0 then
      throw new NoSuchElementException()
    else
      applyHelper(this, 0)
  }

  //--------------------------------------------------------------------------------------
  def length: Int = {
    @tailrec
    def lengthHelper(remaining: EList[T], curLength: Int): Int = remaining match {
      case Empty()      => curLength
      case ::(_, tail)  => lengthHelper(tail, 1 + curLength)
    }
    lengthHelper(this, 0)
  }

  //--------------------------------------------------------------------------------------
  def reverse: EList[T] = {
    @tailrec
    def reverseHelper(remaining: EList[T], acc: EList[T]): EList[T] = remaining match {
      case Empty()        => acc
      case ::(head, tail) => reverseHelper(tail, head :: acc)
    }
    reverseHelper(this, Empty())
  }

  //--------------------------------------------------------------------------------------
  def ++[S >: T](other: EList[S]): EList[S] = this match {
    case Empty() => other
    case ::(head, tail) =>
      @tailrec // implementation identical to reverseHelper
      def concatHelper(remaining: EList[T], acc: EList[S]): EList[S] = remaining match {
        case Empty()        => acc
        case ::(head, tail) => concatHelper(tail, head :: acc)
      }
      concatHelper(this.reverse, other)
  }

  //--------------------------------------------------------------------------------------
  /**
   * Removes the element from the given index. If the index is out of bounds, return the
   * list unaltered.
   */
  def removeAt[S >: T](index: Int): EList[S] =
    if index < 0 then this
    else this match {
      case Empty() => Empty()
      case ::(head, tail) =>
        @tailrec
        def removeAtHelper(remaining: EList[S], curIndex: Int, traversed: EList[S]): EList[S] =
          remaining match {
            case Empty() => this // index is beyond the length of the list
            case ::(head, tail) =>
              if curIndex == index then // remove the current element - the head
                traversed.reverse ++ tail
              else // move on to the next element in the list
                removeAtHelper(tail, curIndex + 1, head :: traversed)
          }
        removeAtHelper(this, 0, Empty())
    }

  //--------------------------------------------------------------------------------------
  def map[U](f: T => U): EList[U] = this match {
    case Empty() => Empty()
    case ::(head, tail) =>
      @tailrec
      def mapHelper(remaining: EList[T], acc: EList[U]): EList[U] = remaining match {
        case Empty()        => acc.reverse
        case ::(head, tail) => mapHelper(tail, f(head) :: acc)
      }
      mapHelper(this, Empty())
  }

  //--------------------------------------------------------------------------------------
  def filter(predicate: T => Boolean): EList[T] = this match {
    case Empty()        => Empty()
    case ::(head, tail) =>
      @tailrec
      def filterHelper(remaining: EList[T], acc: EList[T]): EList[T] = remaining match {
        case Empty()        => acc.reverse
        case ::(head, tail) => 
          val newAcc = if predicate(head) then head :: acc else acc
          filterHelper(tail, newAcc)
      }
      filterHelper(this, Empty())
  }

  //--------------------------------------------------------------------------------------
  // Tail recursive, but has poor performance, as it does ++ at each step. Complexity is O(N^2)
  def flatMap[U](f: T => EList[U]): EList[U] = this match {
    case Empty()        => Empty()
    case ::(head, tail) =>
      @tailrec
      def flatMapHelper(remaining: EList[T], acc: EList[U]): EList[U] = remaining match {
        case Empty()        => acc
        case ::(head, tail) => 
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
        case ::(head, tail) => 
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
  def duplicateEach(count: Int): EList[T] = this match {
    case Empty()        => Empty()
    case ::(head, tail) =>
      @tailrec
      def duplicateEachHelper(remaining: EList[T], times: Int, acc: EList[T]): EList[T] = remaining match {
        case Empty() => acc.reverse
        case ::(head, tail) =>
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
    Shift left k places
    Example: [1,2,3,4,5,6,7,8].rotate(3) == [4,5,6,7,8,1,2,3]
    = rotateHelper([1,2,3,4,5,6,7,8], [], 0)
    = rotateHelper([2,3,4,5,6,7,8], [1], 1)
    = rotateHelper([3,4,5,6,7,8], [2,1], 2)
    = rotateHelper([4,5,6,7,8], [3,2,1], 3)
    = [4,5,6,7,8] ++ [3,2,1].reverse
    = [4,5,6,7,8,1,2,3]
   */
  def rotate(k: Int): EList[T] = this match {
    case Empty() => Empty()
    case ::(head, tail) =>
      @tailrec
      def rotateHelper(remaining: EList[T], right: EList[T], i: Int): EList[T] =
        remaining match {
          case Empty()        => remaining ++ right.reverse
          case ::(head, tail) =>
            if k == i then remaining ++ right.reverse
            else rotateHelper(tail, head :: right, i + 1)
        }
      rotateHelper(this, Empty(), 0)  
  }
  
  //--------------------------------------------------------------------------------------
  // FOLDS
  @tailrec
  final def foldLeft[B](empty: B, f: (T, B) => B): B = this match {
    case Empty()        => empty
    case ::(head, tail) => tail.foldLeft(f(head, empty), f)
  }

  // types:
  // head:      T
  // f:         B
  // foldRight: B
  def foldRight[B](empty: B, f: (T, B) => B): B = this match {
    case Empty()        => empty
    case ::(head, tail) => f(head, tail.foldRight(empty, f))
  }

} // EList

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

    // test get k-th element
    println(aList(2)) // 9
    println(aList(0)) // 7
    // println(aList(5)) // NoSuchElement exception

    // test length
    println(bList.length) // 3
    println(empty.length) // 0

    // test reverse
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
    println(aList.flatMap(x => x :: -x :: 2 * x :: Empty())) // [7, -7, 14, 8, -8, 16, 9, -9, 18, 10, -10, 20]
  }

  //------------------------------------------------------
  val repList = 1 :: 1 :: 1 :: 2 :: 5 :: 5 :: Empty()
  val single  = 100 :: Empty()
  val double  = 7 :: 7 :: Empty()
  
  def testMediumDifficultyProblems() = {
    
    // test run-length encoding
    println(aList.rle) // [(7,1), (8,1), (9,1), (10,1)]
    println(repList.rle) // [(1,3), (2,1), (5,2)]
    println(single.rle) // [(100, 1)]
    println(double.rle) // [(7, 2)]
    
    // test duplicateEach
    println(bList.duplicateEach(3)) // [3, 3, 3, 4, 4, 4, 5, 5, 5]
    println(aList.duplicateEach(2)) // [7, 7, 8, 8, 9, 9, 10, 10]
    
    // test rotate (shift left)
    // aList = [7, 8, 9, 10]
    println(aList.rotate(3)) // [10, 7, 8, 9]
    println(aList.rotate(6)) // [7, 8, 9, 10]
  }
  
  testMediumDifficultyProblems()
}