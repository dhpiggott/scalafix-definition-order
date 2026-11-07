/*
rule = DefinitionOrder
*/
package definitionorder.cycles

def odd(n: Int): Boolean = n != 0 && even(n - 1)

def even(n: Int): Boolean = n == 0 || odd(n - 1)

def check: Boolean = odd(3)
