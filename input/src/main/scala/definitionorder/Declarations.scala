/*
rule = DefinitionOrder
*/
package definitionorder.declarations

trait Shape:
  def describe: String = s"$name measures $area"

  extension (shape: Shape) def scaled(factor: Double): Shape

  val label: String = name.toUpperCase

  def area: Size

  var count: Int

  type Size

  def name: String
