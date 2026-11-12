/*
rule = DefinitionOrder
*/
package definitionorder.types

def describe(shape: Shape): String = shape match
  case Shape.Circle => name
  case Shape.Square => name

enum Shape:
  case Circle, Square

def name: String = "shape"

def main: String = describe(Shape.Circle)
