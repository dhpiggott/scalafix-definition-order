/*
rule = DefinitionOrder
*/
package definitionorder.suppressedpairswap

object Pair: // assert: DefinitionOrder
  // scalafix:off DefinitionOrder
  def two: Pair = Pair(2)
  def one: Pair = Pair(1)
  // scalafix:on DefinitionOrder

// Holds a kept range too, so neither half can move, and the class can't go
// above its companion.
final case class Pair(value: Int):
  // scalafix:off DefinitionOrder
  def second: Int = 2
  def first: Int = 1
  // scalafix:on DefinitionOrder
