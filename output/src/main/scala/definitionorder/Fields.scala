package definitionorder.fields

class Counter:
  var count = 0

  private val step = 1

  def increment(): Unit = count += step

  def reset(): Unit = count = 0
