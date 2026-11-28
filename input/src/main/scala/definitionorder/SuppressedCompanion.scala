/*
rule = DefinitionOrder
*/
package definitionorder.suppressedcompanion

// Holds a kept range, so it stays where it is, and its class moves up to
// sit directly above it.
object Pair:
  // scalafix:off DefinitionOrder
  def second: Int = 2
  def first: Int = 1
  // scalafix:on DefinitionOrder

final case class Pair(value: Int)
