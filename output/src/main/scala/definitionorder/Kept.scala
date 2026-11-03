package definitionorder.kept

def apple: Int = 3

// scalafix:off DefinitionOrder
def second: Int = 2

def first: Int = 1
// scalafix:on DefinitionOrder

def zebra: Int = 1
