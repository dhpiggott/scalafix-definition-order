package definitionorder.nestedfirst

trait First

trait Second

class User:
  def caller: First =
    val _ = callee
    new First {}

  def callee: Second = new Second {}
