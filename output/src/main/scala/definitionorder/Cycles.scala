package definitionorder.cycles

def check: Boolean = odd(3)

def odd(n: Int): Boolean = n != 0 && even(n - 1)

def even(n: Int): Boolean = n == 0 || odd(n - 1)
