/*
rule = DefinitionOrder
*/
package definitionorder.kepttoend

def beta: Int = 2

def alpha: Int = 1

// A scalafix:off with no scalafix:on keeps the rest of the file as written.
// scalafix:off DefinitionOrder
def second: Int = 2

def first: Int = 1
