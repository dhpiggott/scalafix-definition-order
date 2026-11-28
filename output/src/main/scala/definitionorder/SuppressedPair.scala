package definitionorder.suppressedpair

// Holds a kept range, so it stays where it is. So does its companion, and
// entry, which names the type, moves out from between them to below the
// pair.
final case class Pair(value: Int):
  // scalafix:off DefinitionOrder
  def second: Int = 2
  def first: Int = 1
  // scalafix:on DefinitionOrder

object Pair:
  // scalafix:off DefinitionOrder
  def two: Pair = Pair(2)
  def one: Pair = Pair(1)
  // scalafix:on DefinitionOrder

def entry: Int = Pair.one.first
