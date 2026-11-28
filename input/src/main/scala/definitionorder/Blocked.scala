/*
rule = DefinitionOrder
*/
package definitionorder.blocked

def helper: Int = 1 // assert: DefinitionOrder

// A comment that belongs to neither definition.

def entry: Int = helper
