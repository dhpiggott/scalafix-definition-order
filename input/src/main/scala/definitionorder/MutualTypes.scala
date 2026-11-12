/*
rule = DefinitionOrder
*/
package definitionorder.mutualtypes

def helper: Int = 1

final case class Left(right: Option[Right]):
  def size: Int = helper

final case class Right(left: Option[Left])
