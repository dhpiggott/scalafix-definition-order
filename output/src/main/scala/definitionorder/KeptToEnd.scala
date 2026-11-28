package definitionorder.kepttoend

def alpha: Int = 1

def beta: Int = 2

// A scalafix:off with no scalafix:on keeps the rest of the file as written.
// scalafix:off DefinitionOrder
def second: Int = 2

def first: Int = 1
