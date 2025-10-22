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

  def fold[B](f1: A => B, f2: (B, B) => B): B = this match {
    case Leaf(value)        => f1(value)
    case Node(left, right)  => f2(left.fold(f1, f2), right.fold(f1, f2))
  }

  // Implementations using fold
  def sizeF: Int                  = fold(_ => 1, _ + _)
  def containsF(elem: A): Boolean = fold(elem == _, _ || _)

  /* Fold functions construction

    // Skeleton
    def fold[B](): B = this match {
      case Leaf(value)        => ???
      case Node(left, right)  => l.fold ??? r.fold
    }

    // Add parameter for Leaf
    def fold[B](f1: A => B): B = this match {
      case Leaf(value)        => f1(value)
      case Node(left, right)  => l.fold(f1) ??? r.fold(f1)
    }

    // Add parameter for Node
    def fold[B](f1: A => B, f2: (B, B) => B): B = this match {
      case Leaf(value)        => f1(value)
      case Node(left, right)  => f2(left.fold(f1, f2), right.fold(f1, f2))
    }
   */
}

