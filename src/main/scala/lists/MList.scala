package lists

import scala.annotation.tailrec

/**
 * Problems from the book, "Functional Programming Strategies In Scala with Cats" by Noel Welsh
 */

enum MList[A] {
  case Empty()
  case ::(head: A, tail: MList[A])

  def isEmpty: Boolean = this match {
    case Empty()  => true
    case _        => false
  }

  // add an element
  def ::(elem: A): MList[A] = new ::(elem, this)

  // stack-recursive version of length
  def lengthSR: Int = this match {
    case Empty()      => 0
    case ::(_, tail)  => 1 + tail.lengthSR
  }

  // stack -recursive version of contains
  def containsSR(elem: A): Boolean = this match {
    case Empty()        => false
    case ::(head, tail) => (head == elem) || (tail containsSR elem)
  }

  // stack-recursive version of map
  def mapSR[B](f: A => B): MList[B] = this match {
    case Empty()        => Empty()
    case ::(head, tail) => f(head) :: (tail mapSR f)
  }

  @tailrec
  final def foldLeft[B](empty: B, f: (A, B) => B): B = this match {
    case Empty() => empty
    case ::(head, tail) => tail.foldLeft(f(head, empty), f)
  }

  def foldRight[B](empty: B, f: (A, B) => B): B = this match {
    case Empty() => empty
    case ::(head, tail) => f(head, tail.foldRight(empty, f))
  }

  // Implementations using foldLeft
  def lengthFL: Int                 = foldLeft(0, (_, x) => x + 1)
  def containsFL(elem: A): Boolean  = foldLeft(false, (value, prev) => (value == elem) || prev)
  def mapFL[B](f: A => B): MList[B] = foldLeft(Empty(), (elem, acc) => f(elem) :: acc) // doesn't work! reversed

  // Implementations using foldRight
  def mapFR[B](f: A => B): MList[B] = foldRight(Empty(), (elem, result) => f(elem) :: result)

  /* Here's how I came up with the fold functions, following the explanation on pg 36 and 37 of the book.
     From the stack-recursive functions for isEmpty, contains, length and map above, the following pattern emerges.

  def fold[B](): B = this match {
    case Empty()        => ???
    case ::(head, tail) => ??? tail.fold
  }

  This leads to
  def fold[B](empty: B): B = this match {
    case Empty()        => empty
    case ::(head, tail) => ??? tail.fold(empty)
  }

  Add a parameter to combine the head of type A and the recursion whose result is of type B
  It leads to the following solution, which is tail-recursive

  def foldLeft[B](empty: B, f: (A, B) => B): B = this match {
    case Empty()        => empty
    case ::(head, tail) => tail.foldLeft(f(head, empty), f)
  }

  The other solution is foldRight
  def foldRight[B](empty: B, f: (A, B) => B): B = this match {
    case Empty()        => empty
    case ::(head, tail) => f(head, tail.foldRight(empty, f))
  }
   */
 }

object MListTest {

  def main(args: Array[String]): Unit = {

    import MList.*
    val aList: MList[Int] = 7 :: 8 :: 9 :: Empty()

    println(aList.lengthFL)       // 3
    println(aList.containsFL(7))  // true
    println(aList.containsFL(1))  // false

    println(aList.mapFR(x => s"${2 * x}"))  // ["14","16","18"]
  }
}
