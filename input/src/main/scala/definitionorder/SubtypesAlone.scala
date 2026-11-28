/*
rule = DefinitionOrder
*/
package definitionorder.subtypesalone

final case class Leaf(value: Int) extends Tree

sealed trait Tree:
  def isLeaf: Boolean = this match
    case Leaf(_) => true
