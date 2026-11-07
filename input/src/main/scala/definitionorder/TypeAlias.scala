/*
rule = DefinitionOrder
*/
package definitionorder.typealias

object Prices:
  object Money:
    def zero: Money = 0L

  type Money = Long

  def total: Money = Money.zero
