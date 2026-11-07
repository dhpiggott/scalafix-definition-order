/*
rule = DefinitionOrder
*/
package definitionorder.suppressed

object Helpers: // assert: DefinitionOrder
  // Kept as written, so the object can't be moved past entry.
  // scalafix:off DefinitionOrder
  def second: Int = 2
  def first: Int = 1
  // scalafix:on DefinitionOrder

def entry: Int = Helpers.first + Helpers.second
