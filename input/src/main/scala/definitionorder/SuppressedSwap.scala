/*
rule = DefinitionOrder
*/
package definitionorder.suppressedswap

// Both objects hold a kept range, so neither can move, and Second belongs
// below First, which uses it.

object Second: // assert: DefinitionOrder
  // scalafix:off DefinitionOrder
  def b: Int = 2
  def a: Int = 1
  // scalafix:on DefinitionOrder

object First:
  // scalafix:off DefinitionOrder
  def value: Int = Second.a
  // scalafix:on DefinitionOrder
