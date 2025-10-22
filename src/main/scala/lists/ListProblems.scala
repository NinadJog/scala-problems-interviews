package lists

import scala.annotation.tailrec

/**
 * I originally solved some of the problems from the lectures here but later switched to Scala 3's list based on Enums.
 * That's the list named EList in the file ListProblemsEnun.scala. The EList contains the solutions to all the
 * problems and then some.
 */
// Covaraint [+T] means a RList[Dog] is a subtype of RList[Animal]
class Animal
class Dog extends Animal

sealed abstract class RList[+T] { // rhymes with Our List. Parameter is covariant
  def head:       T
  def tail:       RList[T]
  def isEmpty:    Boolean
  def headOption: Option[T]

  /**
   * We introduce S as a supertype of T just so that the code can compile.
   * Otherwise the Scala compiler complains about covariant type being in 
   * contravariant position. This is just a technicality. 
   * 
   * We implement the method here because its implementation is the same 
   * for both RNil and ::(Cons).
   * 
   * This method was initially named prepend, but was refactored to :: 
   * The 'new' is needed on the RHS because if we remove it, the compiler thinks
   * it's a recursive call to the :: method rather than a new Cons (::) instance.
   * 
   * By renaming the prepend method to ::, we have made it right-associative,
   * since Scala allows methods ending with a colon (:) to be right-associative.
   * Therefore, RNil.::(2) == 2 :: RNil
  */
  def ::[S >: T](elem: S): RList[S] = new ::(elem, this)

  /**
   * Easy problems
   */
  // Retrieve the element at index
  def apply(index: Int): T
  def length: Int
  def reverse: RList[T]
  def ++[S >: T](otherList: RList[S]): RList[S]
  // remove an element at the given index; return a new list
  def removeAt(index: Int): RList[T]
}

// Nothing is a substitute for any type.
case object RNil extends RList[Nothing] {
  override def head:       Nothing          = throw new NoSuchElementException
  override def tail:       RList[Nothing]   = throw new NoSuchElementException
  override def isEmpty:    Boolean          = true
  override def headOption: Option[Nothing]  = None
  
  override def toString:   String           = "[]"
  override def apply(index: Int): Nothing   = throw new NoSuchElementException
  override def length: Int                  = 0
  override def reverse: RList[Nothing]      = RNil

  override def ++[S >: Nothing](otherList: RList[S]): RList[S] = otherList

  override def removeAt(index: Int): RList[Nothing] = RNil
}

//----------------------------------------------------------------------------------------------------------------------
// We can override the head and tail methods from the RList supertype by vals in the Cons subtype
// This case class was initially named Cons, but was later refactored to ::
case class ::[+T](override val head: T, override val tail: RList[T]) extends RList[T] {
  override def isEmpty: Boolean = false
  override def headOption: Option[T] = Some(head)

  override def toString: String = {
    @tailrec
    def toStringTailrec(remaining: RList[T], result: String): String = {
      if remaining.isEmpty then
        result
      else if remaining.tail.isEmpty then // only one element remaining
        s"$result${remaining.head}"
      else 
        toStringTailrec(remaining.tail, s"$result${remaining.head}, ")
    }
    
    "[" + toStringTailrec(this, "") + "]"
  }

  // get the list element at index
  override def apply(index: Int): T = {
    @tailrec
    def kthHelper(remaining: RList[T], curIndex: Int): T =
      if curIndex == index then remaining.head
      else kthHelper(remaining.tail, curIndex + 1)

    if index < 0 then throw new NoSuchElementException
    else kthHelper(this, 0)
  }

  override def length: Int = {
    @tailrec
    def lengthHelper(remaining: RList[T], curLength: Int): Int =
      if remaining.isEmpty then curLength
      else
        lengthHelper(remaining.tail, curLength + 1)

    lengthHelper(this, 0)
  }

  override def reverse: RList[T] = {
    @tailrec
    def reverseHelper(remaining: RList[T], result: RList[T]): RList[T] =
      if remaining.isEmpty then result
      else
        reverseHelper(remaining.tail, remaining.head :: result)

    reverseHelper(this, RNil)
  }

  // Stack recursive version is: = head :: (tail ++ otherList)
  override def ++[S >: T](otherList: RList[S]): RList[S] = {
    @tailrec
    def concatHelper(remaining: RList[S], acc: RList[S]): RList[S] =
      if remaining.isEmpty then acc
      else
        concatHelper(remaining.tail, remaining.head :: acc)

    concatHelper(this.reverse, otherList)
    // concatHelper(otherList, this.reverse).reverse // same result, but more steps
  }

  override def removeAt(index: Int): RList[T] = {
    /*
      [4,3,5,2].removeAt(2)
      = removeAtHelper([4,3,5,2], 0, [])
      = removeAtHelper([3,5,2], 1, [4])
      = removeAtHelper([5,2], 2, [3,4])
      = [3,4].reverse ++ [2]
      = [4,3,2]
     */
    @tailrec
    def removeAtHelper(remaining: RList[T], curIndex: Int, acc: RList[T]): RList[T] =
      if remaining.isEmpty then // index is beyond the bounds of this list
        this
      else if curIndex == index then  // omit the element at index
        acc.reverse ++ remaining.tail
      else  // save the head in the accumulator, which contains all the predecessors
        removeAtHelper(remaining.tail, curIndex + 1, remaining.head :: acc)

    if index < 0 then this
    else removeAtHelper(this, 0, RNil)
  }
  /*
    Stack-recursive solution:
    if index == 0 then tail
    else head :: tail.removeAt(index - 1)
   */
}

//----------------------------------------------------------------------------------------------------------------------
// companion object
object RList {
  def from[T](iterable: Iterable[T]): RList[T] = {
    @tailrec
    def fromHelper(remaining: Iterable[T], result: RList[T]): RList[T] =
      if remaining.isEmpty then result
      else
        fromHelper(remaining.tail, remaining.head :: result)

    fromHelper(iterable, RNil).reverse
  }
}

//----------------------------------------------------------------------------------------------------------------------
object ListProblems extends App {
  val aSmallList    = ::(1, ::(2, ::(3, RNil))) // Built using Cons (i.e. the :: case class)
  val aSmallList_v2 = 1 :: 2 :: 3 :: RNil       // same, since :: prepend function is right-associative
  val aSmallList_v3 = RNil.::(3).::(2).::(1)    // same

  val smallList2    = 7 :: 8 :: 9 :: RNil
  val aLargeList    = RList.from(1 to 10000)

  println(aSmallList)     // [1, 2, 3]
  println(aSmallList_v2)  // [1, 2, 3]
  println(aSmallList_v3)  // [1, 2, 3]

  // test get k-th element
  println(aSmallList(1))      // 2
  println(aLargeList(8735))   // 8736

  // test length
  println(aSmallList.length)  // 3
  println(aLargeList.length)  // 10000

  // test reverse
  println(aSmallList.reverse) // [3,2,1]
  // println(aLargeList.reverse) // [10000, 9999,... 2, 1]

  // test concat
  println(aSmallList ++ smallList2) // [1,2,3,7,8,9]

  // test removeAt
  println(aSmallList.removeAt(1)) // [1, 3]
  println(smallList2.removeAt(5)) // [7, 8, 9]

}
