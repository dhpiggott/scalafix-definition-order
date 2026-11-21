package definitionorder.typearguments

trait Wrapper[A]

class Item:
  def boxed: Box = Box()

// Box extends Wrapper and only mentions Item, so Item isn't its supertype:
// the two name each other, and are ordered as two such types are.
class Box extends Wrapper[Item]
