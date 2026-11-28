/*
rule = DefinitionOrder
*/
package definitionorder.visibility

class Basket:
  private[visibility] def apple: Int = 1

  protected def banana: Int = 2

  def cherry: Int = 3
