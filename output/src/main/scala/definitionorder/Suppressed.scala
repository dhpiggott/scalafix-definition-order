package definitionorder.suppressed

def entry: Int = Helpers.first + Helpers.second + Helpers.root

// Holds a kept range, so it stays where it is and entry moves above it. Its
// own body is still ordered around the range.
object Helpers:
  def root: Int = leaf

  def leaf: Int = 0

  // scalafix:off DefinitionOrder
  def second: Int = 2
  def first: Int = 1
  // scalafix:on DefinitionOrder
