/*
rule = DefinitionOrder
*/
package definitionorder.givens

trait Show[A]:
  def show(a: A): String

given defaultShow: Show[Int] with
  def show(a: Int): String = a.toString

def describe(n: Int)(using s: Show[Int]): String = s.show(n)

def render: String = describe(1)
