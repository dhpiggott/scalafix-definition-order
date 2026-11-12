/*
rule = DefinitionOrder
*/
package definitionorder.endmarkers

def helper: Int =
  val x = 1
  x
end helper

object Entry:
  def total: Int =
    helper + extra.twice
  end total
end Entry

extension (n: Int)
  def twice: Int =
    n * 2
  end twice
end extension

def extra: Int =
  val y = 2
  y
end extra // trailing

class Last:
  def value: Int =
    helper
end Last
