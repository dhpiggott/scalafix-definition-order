package definitionorder.subtypesalone

sealed trait Tree:
  def isLeaf: Boolean = this match
    case Leaf(_) => true
    case _       => false

final case class Leaf(value: Int) extends Tree
