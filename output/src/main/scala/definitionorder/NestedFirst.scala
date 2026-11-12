package definitionorder.nestedfirst

trait First

trait Second

class User:
  def caller: First =
    callee
    new First {}

  def callee: Second = new Second {}
