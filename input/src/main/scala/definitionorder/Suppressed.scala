/*
rule = DefinitionOrder
*/
package definitionorder.suppressed

// Holds a kept range, so it stays where it is and entry moves above it. Its
// own body is still ordered around the range.
object Helpers:
  def leaf: Int = 0

  def root: Int = leaf

  // scalafix:off DefinitionOrder
  def second: Int = 2
  def first: Int = 1
  // scalafix:on DefinitionOrder

def entry: Int = Helpers.first + Helpers.second + Helpers.root
