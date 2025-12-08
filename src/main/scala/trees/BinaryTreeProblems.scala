package trees

import scala.annotation.tailrec
import cats.Eval

import scala.collection.immutable.Queue

/*
  Differs from the instructor's version in the following ways.

  1) Uses Scala 3's enum to implement the Tree algebraic data type instead of Scala 2's sealed abstract class.
     Therefore, uses pattern matching instead of if-then-else statements.

  2) Includes stack-recursive implementations that aren't part of the course in addition to the tail-recursive
     solutions required by the course. The stack-recursive solutions assist the implementations using folds.

  3) Includes fold functions and exercises on implementing solutions with fold functions that are
     not part of the course.
*/
enum BTree[+T] {
  case Empty()
  case Node(value: T, left: BTree[T], right: BTree[T])

  // Convenience function used in the tail-recursive implementation of the mirror method
  def isEmpty: Boolean = this match {
    case Empty()  => true
    case _        => false
  }

  /**
   * Easy problems
   */
  def isLeaf: Boolean = this match {
    case Empty()              => false
    case Node(_, left, right) => left == Empty() && right == Empty()
  }

  //--------------------------------------------------------------------------------------
  /*
          _____1____
         /          \
      __2__        __6__
     /     \      /     \
    3       4    7       8
             \
              5

    clh([1],[]) =
    clh([2,6],[]) =
    clh([3,4,6], []) =
    clh([4,6], [3]) =
    clh([5,6], [3]) =
    clh([6], [5,3]) =
    clh([7,8], [5,3]) =
    clh([8], [7,5,3]) =
    clh([], [8,7,5,3]) =
    [8,7,5,3]
   */
  def collectLeavesTR: List[BTree[T]] = {
    @tailrec
    def collectLeavesHelper(pending: List[BTree[T]], leaves: List[BTree[T]]): List[BTree[T]] = pending match {
      case Nil => leaves
      case head :: tail => head match {
        case Empty() => collectLeavesHelper(tail, leaves)
        case node @ Node(_, left, right) =>
          if node.isLeaf then // add node to leaves
            collectLeavesHelper(tail, node :: leaves)
          else  // add left and right subtree nodes to pending
            collectLeavesHelper(left :: right :: tail, leaves)
      }
    }
    collectLeavesHelper(List(this), List())
  }

  def leafCountTR: Int = collectLeavesTR.length

  //--------------------------------------------------------------------------------------
  /**
   * Medium difficulty problems
   */
  def size: Int = {
    @tailrec
    def sizeHelper(pending: List[BTree[T]], count: Int): Int = pending match {
      case Nil => count
      case head :: tail => head match {
        case Empty() => sizeHelper(tail, count)  // don't count the empty node
        case Node(_, left, right) => sizeHelper(left :: right :: tail, count + 1)
      }
    }
    sizeHelper(List(this), 0)
  }

  //--------------------------------------------------------------------------------------
  val sizeAlternative: Int = this match {
    case Empty() => 0
    case Node(_, left, right) => 1 + left.sizeAlternative + right.sizeAlternative
  }

  //--------------------------------------------------------------------------------------
  // collect all the nodes at a given level
  def collectNodes(level: Int): List[BTree[T]] = {

    // Returns the non-empty children of all the levelNodes. My implementation is too verbose.
    // A better implementation is the getChildren method using a for comprehension.
    @tailrec
    def getKids(levelNodes: List[BTree[T]], acc: List[BTree[T]]): List[BTree[T]] = levelNodes match {
      case Nil => acc
      case head :: tail =>
        val newAcc = head match {
          case Empty()                    => acc
          case Node(_, Empty(), Empty())  => acc
          case Node(_, Empty(), right)    => right :: acc
          case Node(_, left, Empty())     => left :: acc
          case Node(_, left, right)       => left :: right :: acc
        }
        getKids(tail, newAcc)
    }

    // Get non-empty children of curNodes. Same functionality as getKids method, but better implementation.
    def getChildren(curNodes: List[BTree[T]]): List[BTree[T]] = for {
      node <- curNodes
      child <- node match {
        case Empty() => List()
        case Node(_, left, right) => List(left, right)
      } if child != Empty()
    } yield child

    @tailrec
    def collectNodesHelper(curLevel: Int, curNodes: List[BTree[T]]): List[BTree[T]] = curNodes match {
      case Nil  => List()
      case _    =>
        if curLevel == level then curNodes
        else {
          collectNodesHelper (curLevel + 1, getChildren(curNodes))
          // collectNodesHelper (curLevel + 1, getKids(curNodes, List())) // Original implementation of mine
        }
    }

    if level < 0 then List()
    else collectNodesHelper(0, List(this))
  }

  //--------------------------------------------------------------------------------------
  // Stack-recursive version of mirror
  def mirrorSR: BTree[T] = this match {
    case Empty() => Empty()
    case Node(v, l, r) => Node(v, r.mirrorSR, l.mirrorSR)
  }

  // Implementation using fold
  def mirrorFoldSR: BTree[T] = this.fold[BTree[T]](Empty()) { (v, l, r) => Node(v, r, l) }

  //--------------------------------------------------------------------------------------
  /*
    Tail-recursive version. Bespoke solution, logically identical to instructor's solution.
    The main difference from instructor's solution is that this one uses pattern matching.
    It's fast, safe, but imperative-style, as it uses an explicit stack instead of the JVM's stack.
  */
  def mirrorTR: BTree[T] = this match {
    case Empty() => Empty()
    case _ =>
      @tailrec
      def mirrorHelper(pending: List[BTree[T]], expanded: Set[BTree[T]],
                       acc: List[BTree[T]]): BTree[T] = pending match {

        // Case 1. pending list is empty, so we are at the end
        case Nil => acc match {
          case Nil => Empty()     // Case 5. Shouldn't occur
          case head :: _ => head  // Case 1. Return the head element from the acc. That's the mirrored tree
        }

        // Case 2. Node is empty or leaf, so remove it from pending & prepend it to accumulator
        case node :: tail if node.isEmpty || node.isLeaf =>
          mirrorHelper(tail, expanded, node :: acc)

        // Case 3. Node has already been visited, so create a new node with the top two elements
        // of the accumulator as its children. Add the new node to the accumulator
        case (node @ Node(value, _, _)) :: tail if expanded.contains(node) =>
          val l :: r :: accTail = acc
          val newAcc = Node(value, l, r) :: accTail
          mirrorHelper(tail, expanded, newAcc)

        // Case 4. Node is not empty, not a leaf, and has not been expanded
        // prepend node's children to pending and add node to visited
        case (node @ Node(_, l, r)) :: tail =>
          mirrorHelper(l :: r :: pending, expanded + node, acc)
      }
      mirrorHelper(List(this), Set(), List())
  }

  //--------------------------------------------------------------------------------------
  // compare the shapes of two trees. stack-recursive solution
  def sameShapeAsSR[S >: T](that: BTree[S]): Boolean = (this, that) match {
    case (Empty(), Empty())                 => true
    case (Empty(), _) | (_, Empty())        => false
    case (Node(_, l1, r1), Node(_, l2, r2)) => (l1 sameShapeAsSR l2) && (r1 sameShapeAsSR r2)
  }

  // tail-recursive version using an explicit stack
  def sameShapeAsTR[S >: T](that: BTree[S]): Boolean = {
    @tailrec
    def loop(stack: List[(BTree[T], BTree[S])]): Boolean = stack match {
      case Nil                        => true         // reached end of list without any mismatches
      case (Empty(), Empty()) :: rest => loop(rest)
      case (Empty(), _) :: _          => false        // mismatch if one node is empty but the other isn't
      case (_, Empty()) :: _          => false
      case (nA, nB) :: rest if nA.isLeaf && nB.isLeaf => loop(rest)
      case (Node(_, l1, r1), Node(_, l2, r2)) :: rest =>  // push children onto stack in any order
        loop((l1, l2) :: (r1, r2) :: rest)
    }
    loop(List((this, that)))
  }

  //--------------------------------------------------------------------------------------
  def isSymmetrical: Boolean = sameShapeAsTR(this.mirrorTR)

  def toListPreorderSR: List[T] = this match {
    case Empty() => List()
    case Node(v, l, r) => (v :: l.toListPreorderSR) ++ r.toListPreorderSR
  }

  // TBD: Ask AI whether the following solution is stack-safe. Even if it's stack-safe,
  // it might have terrible complexity because of the ++ operation at every step.
  def toListPreorderWithEval: Eval[List[T]] = this match {
    case Empty() => Eval.now(List())
    case Node(v, l, r) =>
      for
        left <- Eval.defer(l.toListPreorderWithEval)
        right <- Eval.defer(r.toListPreorderWithEval)
      yield
        (v :: left) ++ right
  }

  // left, right, node
  def toListPostorderSR: List[T] = this match {
    case Empty() => List()
    case Node(v, l, r) => l.toListPostorderSR ++ r.toListPostorderSR ++ List(v)
  }

  // TBD: Ask AI whether the following solution is stack-safe. Even if it's stack-safe,
  // it might have terrible complexity because of the ++ operation at every step.
  def toListPostorderWithEval: Eval[List[T]] = this match {
    case Empty() => Eval.now(List())
    case Node(v, l, r) =>
      for
        left <- l.toListPostorderWithEval
        right <- r.toListPostorderWithEval
      yield
        left ++ right ++ List(v)
  }

  def toListInorderSR: List[T] = this match {
    case Empty() => List()
    case Node(v, l, r) => l.toListInorderSR ++ List(v) ++ r.toListInorderSR
  }

  // TBD: Ask AI whether the following solution is stack-safe. Even if it's stack-safe,
  // it might have terrible complexity because of the ++ operation at every step.
  def toListInorderWithEval: Eval[List[T]] = this match {
    case Empty() => Eval.now(List())
    case Node(v, l, r) =>
      for
        left <- Eval.defer(l.toListInorderWithEval)
        right <- Eval.defer(r.toListInorderWithEval)
      yield
        left ++ List(v) ++ right
  }


  //=========================================================================================
  /* Construction of structural fold functions

  def fold[B](): B = this match {
    case Empty() => ???
    case Node(value, left, right) => ??? left.fold ??? right.fold
  }

  // Add empty parameter
  def fold[B](empty: B): B = this match {
    case Empty() => empty
    case Node(value, left, right) => ??? left.fold(empty) ??? right.fold(empty)
  }

  // Add node parameter
  def fold[B](empty: B)(node: (T, B, B) => B): B = this match {
    case Empty() => empty
    case Node(value, left, right) => value ??? left.fold(empty)(node) ??? right.fold(empty)(node)
  }
  */

  // Standard catamorphism
  def fold[B](empty: B)(node: (T, B, B) => B): B = this match {
    case Empty()       => empty
    case Node(v, l, r) => node(v, l.fold(empty)(node), r.fold(empty)(node))
  }

  // Right-associative variant
  def foldRight[B](empty: B)(node: (T, B, B) => B): B = this match {
    case Empty()       => empty
    case Node(v, l, r) => node(v, r.foldRight(empty)(node), l.foldRight(empty)(node))
  }

  //--------------------------------------------------------------
  // Using the fold functions
  // count nodes
  def nodeCountPreorderFold: Int = this.foldPreorder(0)((_, count) => 1 + count)

  def nodeCount: Int = this.fold(0)((_, l, r) => 1 + l + r)

  // The stack-recursive version's structure mirrors that of the implementation with fold (or rather, vice-versa)
  def nodeCountSR: Int = this match {
    case Empty() => 0
    case Node(_, l, r) => 1 + l.nodeCountSR + r.nodeCountSR
  }

  //--------------------------------------------------------------
  // count leaves - stack-recursive function
  def leafCountSR: Int = this match {
    case Empty()                    => 0
    case Node(_, Empty(), Empty())  => 1
    case Node(_, l, r)              => l.leafCountSR + r.leafCountSR
  }

  // Implementation with fold
  def leafCount: Int = this.fold(0) { (_, l, r) =>
    if (l == 0) && (r == 0) then 1 else l + r
  }

  // tree depth
  def depth: Int = this.fold(0) { (_, l, r) =>
    1 + (l max r)
  }

  //--------------------------------------------------------------
  /*
    This preorder tail-recursive fold is perfect for traversal-based operations but is insufficient for
    structural analysis of trees. Use it for traversals, searching, node-value transformations & aggregations.

    It cannot directly calculate most tree-structure attributes such as number of leaves, depth, or size.

    What it can't do:
    1. Number of leaves (can't distinguish leaves from internal nodes)
    2. Tree depth/height (loses level information)
    3. Number of nodes (could count, but inefficiently)

    It fails for structural attributes because its function signature (T, B) => B only has access to:
    * Current node's value T
    * Accumulated result so far B
    It lacks access to subtree results because it processes nodes linearly in preorder without
    building the tree structure back up.
   */
  def foldPreorder[B](init: B)(f: (T, B) => B): B = {
    @tailrec
    def loop(stack: List[BTree[T]], acc: B): B =
      stack match {
        case Nil                    => acc
        case Empty() :: rest        => loop(rest, acc) // skip empty node
        case (node @ Node(v, l, r)) :: rest  =>
          // Preorder: process value first
          val newAcc = f(v, acc)
          // Pop node from stack. If it's not a leaf, push its two children.
          // Push right then left so that left is processed first
          val newStack = if node.isLeaf then rest else l :: r :: rest
          loop(newStack, newAcc)
      }
    loop(List(this), init)
  }

  /*
          _____1____
         /          \
      __2__        __6__
     /     \      /     \
    3       4    7       8
             \
              5

  foldInorder([1], (v, acc) => v :: acc) =
  loop(1,     [],       []) =
  loop(2,     [1],      []) =
  loop(3,     [2,1],    []) =
  loop(Empty, [3,2,1],  []) =
  loop(Empty, [2,1],    [3]) =
  loop(4,     [1],      [2,3]) =
  loop(Empty, [4,1],    [2,3]) =
  loop(5,     [1],      [4,2,3]) =
  loop(Empty, [5,1],    [4,2,3]) =
  loop(Empty, [1],      [5,4,2,3]) =
  loop(6,     [],       [1,5,4,2,3] =         // Case 3
  loop(7,     [6],      [1,5,4,2,3] =         // Case 3
  loop(Empty, [7,6],    [1,5,4,2,3] =         // Case 3
  loop(Empty, [6],      [7,1,5,4,2,3] =       // Case 2
  loop(8,     [],       [6,7,1,5,4,2,3] =     // Case 2
  loop(Empty, [8],      [6,7,1,5,4,2,3] =     // Case 3
  loop(Empty, [],       [8,6,7,1,5,4,2,3] =   // Case 2
  ...
  [8,6,7,1,5,4,2,3].reverse =
  [3,2,4,5,1,7,6,8]
  */
  def foldInorder[B](init: B)(f: (T, B) => B): B = {
    @tailrec
    def loop(cur: BTree[T], stack: List[BTree[T]], acc: B): B = cur match {
      case Empty() => stack match {
        case Nil => acc                 // Case 1
        case Node(v, _, r) :: rest =>   // Case 2
          // Pop the node. Process its value, then move to its right child
          val newAcc = f(v, acc)
          loop(r, rest, newAcc)
      }

      // Walk leftwards, pushing nodes onto stack - Case 3
      case Node(v, l, _) => loop(l, cur :: stack, acc)
    }
    loop(this, List(), init)
  }

  // Using the iterative fold (traversal fold)
  def toListPreorderWithFold: List[T] =
    foldPreorder[List[T]](List()) { (v, acc) => v :: acc }
      .reverse

  def toListInorderWithFold: List[T] =
    foldInorder[List[T]](List()) { (v, acc) => v :: acc}
      .reverse

  //-----------------------------------------------------
  // Per-level (breadth-first search) traversal

  // My implementation. Works, but is verbose
  def toListPerLevel: List[T] = this match {
    case Empty() => List()
    case Node(v, _, _) =>
      // Get non-empty children of curNodes.
      def getChildren(curNodes: List[BTree[T]]): List[BTree[T]] = for {
        node <- curNodes
        child <- node match {
          case Empty() => List()
          case Node(_, left, right) => List(right, left)
        } if child != Empty()
      } yield child

      @tailrec
      def loop(curNodes: List[BTree[T]], acc: List[List[T]]): List[T] = curNodes match {
        case Nil => acc.flatten.reverse
        case _ =>
          val kids = getChildren(curNodes)
          val kidValues = kids map { case Node(v, _, _) => v }
          loop(kids, kidValues :: acc)
      }
      loop(List(this), List(List(v)))
  }

  /*
          _____1____
         /          \
      __2__        __6__
     /     \      /     \
    3       4    7       8
             \
              5

    // Answer should be [1,2,6,3,4,7,8,5]
    // Queue is shown in <>. Items get enqueued from the right and dequeued from the left.

    bfs(<1>,        []) =
    bfs(<2,6>,      [1]) =      // 2 will get dequeued
    bfs(<6,3,4>     [2,1]) =    // 6 will get dequeued
    bfs(<3,4,7,8>,  [6,2,1]) =
    bfs(<4,7,8>,    [3,6,2,1]) =
    bfs(<7,8,5>,    [4,3,6,2,1]) =
    bfs(<8,5>,      [7,4,3,6,2,1]) =
    bfs(<5>,        [8,7,4,3,6,2,1]) =
    bfd(<>,         [5,8,7,4,3,6,2,1]) =
    [5,8,7,4,3,6,2,1].reverse =
    [1,2,6,3,4,7,8,5]

    A queue rather than a list is a natural choice for breadth-first search.
  */
  def toListPerLevelWithQueue: List[T] = this match {
    case Empty() => List()  // degenerate case of an empty tree

    case n0: Node[T] =>  // Alternative syntax: n0 @ Node(_,_,_)
      // Insert only non-empty children into a Node-only queue
      def enqueueChildren(q: Queue[Node[T]], left: BTree[T], right: BTree[T]): Queue[Node[T]] = (left, right) match {
        case (l: Node[T], r: Node[T]) => q.enqueue(l).enqueue(r)
        case (l: Node[T], _)          => q.enqueue(l) // right child is Empty
        case (_, r: Node[T])          => q.enqueue(r) // left child is Empty
        case _                        => q            // both children are empty
      }

      /* Same helper function with alternative syntax in the cases.
      def enqueueChildren(q: Queue[Node[T]], left: BTree[T], right: BTree[T]): Queue[Node[T]] = (left, right) match {
        case (l@Node(_, _, _), r@Node(_, _, _)) => q.enqueue(l).enqueue(r) // l and r are refined to type Node
        case (l@Node(_, _, _), _) => q.enqueue(l)
        case (_, r@Node(_, _, _)) => q.enqueue(r)
        case _ => q // both children are empty
      }*/

      @tailrec
      def bfs(q: Queue[Node[T]], acc: List[T]): List[T] = q.dequeueOption match {
        case None                     => acc.reverse
        case Some(Node(v, l, r), qs)  =>
          val nonEmptyChildren = List(l, r).collect { case n: Node[T] => n }
          bfs(qs.enqueueAll(nonEmptyChildren), v :: acc)
          // bfs(enqueueChildren(qs, l, r), v :: acc) // Earlier implementation; also works
      }
      bfs(Queue(n0), List())
  }

  // Using the anamorphism List.unfold. From AI
  // TBD: Understand how it works; go step-by-step with a real example
  def toListLevelOrderUnfold: List[T] = List.unfold(List(this)) {
    case Nil => None
    case nodes =>
      nodes.collect { case Node(v, _, _) => v } match {
        case Nil => None
        case values =>
          val children = nodes.flatMap {
            case Node(_, l, r) => List(l, r)
            case _ => List()
          }
          Some(values, children)
      }
  }.flatten
}

//=========================================================================================
// Application

object BinaryTreeProblems extends App {
  import BTree.*

  // Convenience method for showing the values of a tree node
  def extractNodeValues[T](list: List[BTree[T]]): List[T] =
    list map {
      case Node(value, _, _)  => value
    }

  // Sum the values at all the nodes in an integer tree
  def sumSR(tree: BTree[Int]): Int = tree match {
    case Empty()                  => 0
    case Node(value, left, right) => value + sumSR(left) + sumSR(right)
  }

  // Implementation using fold
  def sum(tree: BTree[Int]): Int = tree.fold(0)((v, l, r) => v + l + r)

  // Implementation with tail-recursive preorder fold
  def sumPreorderFoldTR(tree: BTree[Int]): Int = tree.foldPreorder(0)(_ + _)

  //---------------------------------------------------------------------------
  val tree =
    Node(1,
      Node(2,
        Node(3, Empty(), Empty()),
        Node(4,
          Empty(),
          Node(5, Empty(), Empty())
        )
      ),
      Node(6,
        Node(7, Empty(), Empty()),
        Node(8, Empty(), Empty())
      )
    )

  val treeSameShape =
    Node(21,
      Node(35,
        Node(49, Empty(), Empty()),
        Node(14,
          Empty(),
          Node(28, Empty(), Empty())
        )
      ),
      Node(6,
        Node(36, Empty(), Empty()),
        Node(12, Empty(), Empty())
      )
    )  

  val simpleTree =
    Node(1,
      Node(2, Empty(), Empty()),
      Node(3, Empty(), Empty())
    )

  val leftSimpleTree  = Node(1, Node(2, Empty(), Empty()),Empty())
  val leftSimpleTree2 = Node(9, Node(7, Empty(), Empty()),Empty())

  /**
   * Easy problems + Implementations with Folds
   */
  val leaves = extractNodeValues(tree.collectLeavesTR)
  println(s"leaves          = $leaves")               // List(8,7,5,3)

  // leaf counts
  println(s"leaf count TR   = ${tree.leafCountTR}")  // 4
  println(s"leaf count SR   = ${tree.leafCountSR}")  // 4
  println(s"leaf count Fold = ${tree.leafCount}")   // 4

  // node counts
  println(s"node count SR            = ${tree.nodeCountSR}")            // 8
  println(s"node count Fold          = ${tree.nodeCount}")              // 8
  println(s"node count Preorder Fold = ${tree.nodeCountPreorderFold}")  // 8

  // sum of node values
  println(s"node value sum SR            = ${sumSR(tree)}") // 36
  println(s"node value sum Fold          = ${sum(tree)}")   // 36
  println(s"node value sum Preorder Fold = ${sumPreorderFoldTR(tree)}")   // 36

  /**
   * Medium-difficulty problems
   */
  // test size
  println(s"tree size     = ${tree.size}")       // 8
  println(s"tree size alt = ${tree.sizeAlternative}")

  // tree of size 100000
  val degenerateTree = (1 to 100000).foldLeft[BTree[Int]] (Empty()) { (tree, number) => Node(number, tree, Empty()) }
  println(s"degenerate tree size     = ${degenerateTree.size}")             // 100,000
  println(s"degenerate tree size alt = ${degenerateTree.sizeAlternative}")  // 100,000

  // test collectNodes
  for (i <- 0 to 4) {
    val levelNodes = extractNodeValues(tree.collectNodes(i))
    println(s"nodes at level $i = $levelNodes")
  }
  /* Output:
      nodes at level 0 = List(1)
      nodes at level 1 = List(2, 6)
      nodes at level 2 = List(3, 4, 7, 8)
      nodes at level 3 = List(5)
      nodes at level 4 = List()
   */

  // test mirror
  println(tree.mirrorSR)
  println(tree.mirrorTR)
  println(tree.mirrorFoldSR)  // implementation with fold
  /*
      1
     / \
    6   2
   / \ / \
  8  7 4  3
      /
     5
  */
  
  // test sameShapeAs
  println("\nsameShapeAs")
  println(leftSimpleTree.sameShapeAsSR(leftSimpleTree2))  // true
  println(tree.sameShapeAsSR(treeSameShape))              // true

  println(tree.sameShapeAsSR(leftSimpleTree))             // false
  println(treeSameShape.sameShapeAsSR(leftSimpleTree2))   // false

  // test isSymmetrical
  println("\nisSymmetrical")
  println(simpleTree.isSymmetrical) // true
  println(tree.isSymmetrical)       // false

  /*
        __1__
       /     \
      2       6
     / \     / \
    3   4   7   8
         \
          5
   */
  println("\nPreorder Traversal")
  println(tree.toListPreorderSR)  // [1,2,3,4,5,6,7,8]
  println(tree.toListPreorderWithFold) // [1,2,3,4,5,6,7,8]
  println(tree.toListPreorderWithEval.value) // [1,2,3,4,5,6,7,8]

  println("\nPostorder Traversal")
  println(tree.toListPostorderSR) // [3,5,4,2,7,8,6,1]
  println(tree.toListPostorderWithEval.value) // [3,5,4,2,7,8,6,1]

  println("\nInorder Traversal")
  println(tree.toListInorderSR)             // [3,2,4,5,1,7,6,8]
  println(tree.toListInorderWithFold)       // [3,2,4,5,1,7,6,8]
  println(tree.toListInorderWithEval.value) // [3,2,4,5,1,7,6,8]

  println("\nTraversal Per Level")
  println(tree.toListPerLevel)              // [1,2,6,3,4,7,8,5]
  println(tree.toListPerLevelWithQueue)     // [1,2,6,3,4,7,8,5]
  println(tree.toListLevelOrderUnfold)      // [1,2,6,3,4,7,8,5]

}
