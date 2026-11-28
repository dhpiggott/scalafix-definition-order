package definitionorder.endmarkers

object Entry:
  def total: Int =
    helper + extra.twice
  end total
end Entry

def extra: Int =
  val y = 2
  y
end extra // trailing

extension (n: Int)
  def twice: Int =
    n * 2
  end twice
end extension

class Last:
  def value: Int =
    helper
end Last

def helper: Int =
  val x = 1
  x
end helper
