/*
rule = DefinitionOrder
*/
package definitionorder.nestedfirst

trait Second

trait First

class User:
  def callee: Second = new Second {}
  def caller: First =
    callee
    new First {}
