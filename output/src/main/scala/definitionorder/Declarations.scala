package definitionorder.declarations

trait Shape:
  type Size

  def area: Size

  def name: String

  val label: String = name.toUpperCase

  def describe: String = s"$name measures $area"
