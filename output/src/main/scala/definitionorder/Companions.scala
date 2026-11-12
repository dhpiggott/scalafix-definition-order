package definitionorder.companions

final case class Box(size: Int)

object Box:
  def empty: Box = Box(0)

def make: Box = Box.empty
