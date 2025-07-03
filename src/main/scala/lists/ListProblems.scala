package lists

import scala.annotation.tailrec

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
}

// Nothing is a substitute for any type.
case object RNil extends RList[Nothing] {
  override def head:       Nothing         = throw new NoSuchElementException()
  override def tail:       RList[Nothing]  = throw new NoSuchElementException()
  override def isEmpty:    Boolean         = true
  override def headOption: Option[Nothing] = None
  
  override def toString:   String          = "[]"
}

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
}

object ListProblems extends App {
  val aSmallList    = ::(1, ::(2, ::(3, RNil))) // Built using Cons (i.e. the :: case class)
  val aSmallList_v2 = 1 :: 2 :: 3 :: RNil       // same, since :: prepend function is right-associative
  val aSmallList_v3 = RNil.::(3).::(2).::(1)    // same
  
  println(aSmallList)     // [1, 2, 3]
  println(aSmallList_v2)  // [1, 2, 3]
  println(aSmallList_v3)  // [1, 2, 3]

}
