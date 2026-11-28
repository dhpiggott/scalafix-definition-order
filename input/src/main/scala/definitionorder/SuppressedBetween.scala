/*
rule = DefinitionOrder
*/
package definitionorder.suppressedbetween

// Holds a kept range, so it stays where it is. Its companion still moves,
// to directly below it, and entry, which names the type, moves out from
// between them to below the pair.
final case class Pair(value: Int):
  // scalafix:off DefinitionOrder
  def second: Int = 2
  def first: Int = 1
  // scalafix:on DefinitionOrder

def entry: Int = Pair.one.first

object Pair:
  def one: Pair = Pair(1)
