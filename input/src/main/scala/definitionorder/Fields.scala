/*
rule = DefinitionOrder
*/
package definitionorder.fields

class Counter:
  def increment(): Unit = count += step

  def reset(): Unit = count = 0

  private val step = 1

  var count = 0
