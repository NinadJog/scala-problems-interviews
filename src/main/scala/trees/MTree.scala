package trees

/**
 * Problems from the book, "Functional Programming Strategies In Scala with Cats" by Noel Welsh
*/

enum MTree[A] {
  case Leaf(value: A)
  case Node(left: MTree[A], right: MTree[A])

  // counts the number of leaves
  def sizeSR: Int = this match {
    case Leaf(_)    => 1
    case Node(l, r) => l.sizeSR + r.sizeSR
  }

  // returns true if the tree contains the given element
  def containsSR(elem: A): Boolean = this match {
    case Leaf(value)  => elem == value
    case Node(l, r)   => (l containsSR elem) || (r containsSR elem)
  }

  def mapSR[B](f: A => B): MTree[B] = this match {
    case Leaf(value)  => Leaf(f(value))
    case Node(l, r)   => Node(l mapSR f, r mapSR f)
  }

  def fold[B](leaf: A => B)(node: (B, B) => B): B = this match {
    case Leaf(value)        => leaf(value)
    case Node(left, right)  => node(left.fold(leaf)(node), right.fold(leaf)(node))
  }

  // Implementations using fold
  def sizeF: Int                    = fold(_ => 1)(_ + _)
  def containsF(elem: A): Boolean   = fold(elem == _)(_ || _)
  def mapF[B](f: A => B): MTree[B]  = fold { v => Leaf(f(v)) }{ (l, r) => Node(l, r) }

  /* Fold function construction

    // Skeleton
    def fold[B](): B = this match {
      case Leaf(value)        => ???
      case Node(left, right)  => left.fold ??? right.fold
    }

    // Add parameter for Leaf
    def fold[B](leaf: A => B): B = this match {
      case Leaf(value)        => leaf(value)
      case Node(left, right)  => left.fold(f1) ??? right.fold(f1)
    }

    // Add parameter for Node
    def fold[B](leaf: A => B, node: (B, B) => B): B = this match {
      case Leaf(value)        => leaf(value)
      case Node(left, right)  => node(left.fold(leaf)(node), right.fold(leaf)(node))
    }
   */
}

