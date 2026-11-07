package definitionorder.subtypes

sealed trait Tree:
  def isLeaf: Boolean = this match
    case Leaf(_) => true
    case _       => false

  def size: Int

def count(tree: Tree): Int = tree.size

final case class Leaf(value: Int) extends Tree:
  def size: Int = 1
