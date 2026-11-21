/*
rule = DefinitionOrder
*/
package definitionorder.typearguments

// Box extends Wrapper and only mentions Item, so Item isn't its supertype:
// the two name each other, and are ordered as two such types are.
class Box extends Wrapper[Item]

class Item:
  def boxed: Box = Box()

trait Wrapper[A]
