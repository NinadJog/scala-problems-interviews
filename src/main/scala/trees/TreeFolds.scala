package trees

/*
  Fold functions and implementations of algorithms using the folds.
  Contains both structural folds (catamorphisms) and traversal folds (iterative folds)
  and their tail-recursive versions.
  This material supplements what's taught in the RockTheJVM course.
 */
import BTree.*
import cats.Eval
import scala.annotation.tailrec

extension [T](tree: BTree[T]) {

  // FOUNDATIONAL FUNCTIONS

  //--------------------------------------------------------------------------------------
  // Stack-recursive structural fold (with natural recursion). Do not use in production
  // Risk of stack overflow unless we use deferred computations with Cats.Eval
  def foldSR[B](empty: B)(node: (T, B, B) => B): B = tree match {
    case Empty()       => empty
    case Node(v, l, r) => node(v, l.fold(empty)(node), r.fold(empty)(node))
  }

  //--------------------------------------------------------------------------------------
  /*
    Using cats.Eval. This is the canonical approach for stack-safe recursion when we
    want the logical structure of the recursive fold without rewriting it to use an explicit stack.
   */
  def foldEval[B](empty: B)(node: (T, B, B) => B): Eval[B] = tree match {
    case Empty()        => Eval.now(empty)
    case Node(v, l, r)  =>
      // defer recursion for stack safety
      for
        leftVal <- Eval.defer(l.foldEval(empty)(node))
        rightVal <- Eval.defer(r.foldEval(empty)(node))
      yield node(v, leftVal, rightVal)
  }

  //--------------------------------------------------------------------------------------
  // Same fold implemented in a tail-recursive manner. Structural fold
  def foldTR[B](empty: B)(node: (T, B, B) => B): B = {

    enum Frame {
      case Visit(t: BTree[T]) // marks tree nodes that have been visited
      case Combine(v: T)      // to combine values
    }

    @tailrec
    def loop(stack: List[Frame], acc: List[B]): B = stack match {
      case Nil => acc.head

      case Frame.Visit(Empty()) :: rest =>
        loop(rest, empty :: acc)

      case Frame.Visit(Node(v, l, r)) :: rest =>
        // visit left, then right, then combine
        val newStack = Frame.Visit(l) :: Frame.Visit(r) :: Frame.Combine(v) :: rest
        loop(newStack, acc)

      case Frame.Combine(v) :: rest =>
        // call the node function
        val rightVal :: leftVal :: tail = acc
        loop(rest, node(v, leftVal, rightVal) :: tail)
    }
    loop(List(Frame.Visit(tree)), Nil)
  }

  //--------------------------------------------------------------------------------------
  // mirror using the tail-recursive fold (best solution)
  def mirrorFoldTR: BTree[T] =
    tree.foldTR[BTree[T]](Empty()) { (v, l, r) => Node(v, r, l) }

  // mirror using cats.Eval. Uses recursive fold with stack-safe recursion
  def mirrorFoldEval: Eval[BTree[T]] =
    tree.foldEval[BTree[T]](Empty()) { (v, l, r) => Node(v, r, l) }

  //--------------------------------------------------------------------------------------
  // compare the shapes of two trees. stack-recursive solution
  // avoids stack overflow because Eval.defer yields a trampolined computation
  def sameShapeAsEval[S >: T](that: BTree[S]): Eval[Boolean] = {
    (tree, that) match {
      case (Empty(), Empty())           => Eval.now(true)
      case (Empty(), _) | (_, Empty())  => Eval.now(false)
      case (Node(_, l1, r1), Node(_, l2, r2)) =>
        for 
          left <- Eval.defer(l1 sameShapeAsEval l2)
          right <- Eval.defer(r1 sameShapeAsEval r2)
        yield
          left && right
    }
  }
  
}

/*
  TBD:
  1. Implement mirror with the foldTR
  2. Implement sameShapeAs

*/

object TreeFolds {

}
