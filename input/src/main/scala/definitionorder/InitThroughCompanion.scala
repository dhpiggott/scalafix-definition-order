/*
rule = DefinitionOrder
*/
package definitionorder.initthroughcompanion

// Touching Rate initialises its companion, which reads base, so base has to
// be initialised before total.
val total = Rate.default

final case class Rate(value: Int)

object Rate:
  val default: Rate = Rate(base)

val base = 1
