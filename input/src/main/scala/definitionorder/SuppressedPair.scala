/*
rule = DefinitionOrder
*/
package definitionorder.suppressedpair

final case class Pair(value: Int): // assert: DefinitionOrder
  // scalafix:off DefinitionOrder
  def second: Int = 2
  def first: Int = 1
  // scalafix:on DefinitionOrder

def entry: Int = Pair.one.first

// Holds a kept range too, so neither half can move, and entry is stuck
// between them.
object Pair:
  // scalafix:off DefinitionOrder
  def two: Pair = Pair(2)
  def one: Pair = Pair(1)
  // scalafix:on DefinitionOrder
