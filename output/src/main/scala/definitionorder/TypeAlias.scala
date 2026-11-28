package definitionorder.typealias

object Prices:
  type Money = Long

  def total: Money = Money.zero

  object Money:
    def zero: Money = 0L
