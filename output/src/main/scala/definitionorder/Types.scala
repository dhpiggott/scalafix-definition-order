package definitionorder.types

enum Shape:
  case Circle, Square

def main: String = describe(Shape.Circle)

def describe(shape: Shape): String = shape match
  case Shape.Circle => name
  case Shape.Square => name

def name: String = "shape"
