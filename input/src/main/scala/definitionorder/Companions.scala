/*
rule = DefinitionOrder
*/
package definitionorder.companions

object Box:
  def empty: Box = Box(0)

final case class Box(size: Int)

def make: Box = Box.empty
