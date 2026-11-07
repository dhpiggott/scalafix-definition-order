/*
rule = DefinitionOrder
*/
package definitionorder.kept

def zebra: Int = 1

// scalafix:off DefinitionOrder
def second: Int = 2

def first: Int = 1
// scalafix:on DefinitionOrder

def apple: Int = 3
