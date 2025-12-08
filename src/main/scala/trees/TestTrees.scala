package trees

/*
  Test all the tree problems from BinaryTreeProblems as well as the folds from TreeFolds
 */

import BTree.*

object TestTrees extends App {

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

  val leftSimpleTree = Node(1, Node(2, Empty(), Empty()), Empty())
  val leftSimpleTree2 = Node(9, Node(7, Empty(), Empty()), Empty())

  // test mirror
  println(tree.mirrorTR)              // bespoke solution based on instructor's solution
  println(tree.mirrorSR)              // stack-recursive; not stack-safe
  println(tree.mirrorFoldSR)          // implementation with stack-recursive fold
  println(tree.mirrorFoldTR)          // second-best: with tail-recursive fold
  println(tree.mirrorFoldEval.value)  // best: with Cats.eval. Naturally recursive logic but stack-safe
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
  println(tree.sameShapeAsSR(treeSameShape))          // true
  println(tree.sameShapeAsTR(treeSameShape))          // true
  println(tree.sameShapeAsEval(treeSameShape).value)  // true

  println(tree.sameShapeAsSR(leftSimpleTree))         // false
  println(tree.sameShapeAsTR(leftSimpleTree))         // false
  println(tree.sameShapeAsEval(leftSimpleTree).value) // false

}
