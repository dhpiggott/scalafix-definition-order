package definitionorder.declarations

trait Shape:
  type Size

  def area: Size

  var count: Int

  def name: String

  extension (shape: Shape) def scaled(factor: Double): Shape

  val label: String = name.toUpperCase

  def describe: String = s"$name measures $area"
