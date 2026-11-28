package definitionorder.mutualtypes

final case class Right(left: Option[Left])

final case class Left(right: Option[Right]):
  def size: Int = helper

def helper: Int = 1
