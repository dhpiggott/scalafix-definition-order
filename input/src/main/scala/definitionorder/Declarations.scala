/*
rule = DefinitionOrder
*/
package definitionorder.declarations

trait Shape:
  def describe: String = s"$name measures $area"

  val label: String = name.toUpperCase

  def area: Size

  type Size

  def name: String
