package definitionorder.givens

def render: String = describe(1)

trait Show[A]:
  def show(a: A): String

def describe(n: Int)(using s: Show[Int]): String = s.show(n)

given defaultShow: Show[Int] with
  def show(a: Int): String = a.toString
