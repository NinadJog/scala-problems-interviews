package trees

import scala.annotation.tailrec

enum BTree[+T] {
  case Empty()
  case Node(value: T, left: BTree[T], right: BTree[T])

  // Convenience function used in the mirror method
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
  def mirror: BTree[T] = this.fold[BTree[T]](Empty()) { (v, l, r) => Node(v, r, l) }

  //--------------------------------------------------------------------------------------
  // Tail-recursive version
  def mirrorTR: BTree[T] = this match {
    case Empty() => Empty()
    case _ =>
      @tailrec
      def mirrorHelper(pending: List[BTree[T]], visited: Set[BTree[T]],
                       acc: List[BTree[T]]): BTree[T] = pending match {

        // Case 1. pending list is empty, so we are at the end
        case Nil => acc match {
          case Nil => Empty()     // Case 5. Shouldn't occur
          case head :: _ => head  // Case 1. Return the head element from the acc. That's the mirrored tree
        }

        // Case 2. Node is empty or leaf, so remove it from pending & prepend it to accumulator
        case node :: tail if node.isEmpty || node.isLeaf =>
          mirrorHelper(tail, visited, node :: acc)

        // Case 3. Node has already been visited, so create a new node with the top two elements
        // of the accumulator as its children. Add the new node to the accumulator
        case (node @ Node(value, _, _)) :: tail if visited.contains(node) =>
          val newAcc = acc match {
            case l :: r :: accTail => Node(value, l, r) :: accTail
          }
          mirrorHelper(tail, visited, newAcc)

        // Case 4. Node is not empty, not a leaf, and has not been visited
        // prepend node's children to pending and add node to visited
        case (node @ Node(_, left, right)) :: tail =>
          mirrorHelper(left :: right :: pending, visited + node, acc)
      }
      mirrorHelper(List(this), Set(), List())
  }

  //=========================================================================================
  /* Fold function construction

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
    case Empty()                  => empty
    case Node(value, left, right) => node(value, left.fold(empty)(node), right.fold(empty)(node))
  }

  // Right-associative variant
  def foldRight[B](empty: B)(node: (T, B, B) => B): B = this match {
    case Empty()                  => empty
    case Node(value, left, right) => node(value, right.foldRight(empty)(node), left.foldRight(empty)(node))
  }

  // count nodes
  def nodeCount: Int = this.fold(0)((_, l, r) => 1 + l + r)

  // The stack-recursive version's structure mirrors that of the implementation with fold (or rather, vice-versa)
  def nodeCountSR: Int = this match {
    case Empty() => 0
    case Node(_, l, r) => 1 + l.nodeCountSR + r.nodeCountSR
  }

  // count leaves - stack-recursive function
  def leafCountSR: Int = this match {
    case Empty()                    => 0
    case Node(_, Empty(), Empty())  => 1
    case Node(_, l, r)              => l.leafCountSR + r.leafCountSR
  }

  // Implementation with fold
  def leafCount: Int = this.fold(0)((_, l, r) => if (l == 0) && (r == 0) then 1 else l + r)

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

  val simpleTree =
    Node(1,
      Node(2, Empty(), Empty()),
      Node(3, Empty(), Empty())
    )

  val leftSimpleTree = Node(1, Node(2, Empty(), Empty()),Empty())

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
  println(s"node count SR   = ${tree.nodeCountSR}") // 8
  println(s"node count Fold = ${tree.nodeCount}")   // 8

  // sum of node values
  println(s"node value sum SR   = ${sumSR(tree)}") // 36
  println(s"node value sum Fold = ${sum(tree)}")   // 36

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
  println(tree.mirror)  // implementation with fold
  /*
      1
     / \
    6   2
   / \ / \
  8  7 4  3
      /
     5
  */
}
