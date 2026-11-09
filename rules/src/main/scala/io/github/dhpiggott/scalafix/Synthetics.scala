package io.github.dhpiggott.scalafix

import scalafix.v1.*

private[scalafix] object Synthetics {

  final case class Occurrence(offset: Int, symbol: Symbol)

  // Uses the source never spells out. In the Givens fixture,
  //
  //   given defaultShow: Show[Int] with ...
  //   def describe(n: Int)(using s: Show[Int]): String = s.show(n)
  //   def render: String = describe(1)
  //
  // `render` uses `defaultShow`, because the compiler turns `describe(1)`
  // into `describe(1)(using defaultShow)`, but nothing in `render`'s source
  // names it. SemanticDB records each such insertion as a synthetic: a tree
  // of what was inserted (`defaultShow`), with the code it was attached to
  // (`describe(1)`) inside it. Uses are found by position: a name written
  // in the source is used by whichever definition's text contains it, at
  // the point it's written. An inserted symbol has no position of its own,
  // so it takes the position where the code it was attached to starts, as
  // if `defaultShow` were written at the start of `describe(1)`. That puts
  // the use inside `render`, so `defaultShow` goes below `render`, and
  // among `render`'s uses it comes where `describe(1)` does. Without this,
  // a given used only implicitly would have no users at all. A synthetic
  // can wrap several pieces of written code: Scala 2 records
  // `for (x <- xs; y <- ys) yield (x, y)` as `xs.flatMap(x => ys.map(...))`,
  // with `xs`, `ys` and `(x, y)` as originals. They all lie in one
  // definition, so which is used decides only where the use ranks among
  // that definition's others. The first as written is used: `originsOf`
  // looks in an application's function before its arguments.
  def inferredUses(doc: SemanticDocument): List[Occurrence] =
    doc.synthetics.toList.flatMap { synthetic =>
      originsOf(synthetic).headOption.toList.flatMap { origin =>
        symbolsIn(synthetic).map(Occurrence(origin, _))
      }
    }

  // Where in the source a synthetic was inserted: the start of the
  // `OriginalTree` or `OriginalSubTree` inside it, which stands for the code
  // as written. The other cases only look further in.
  private def originsOf(tree: SemanticTree): List[Int] = tree match {
    case OriginalTree(original)        => List(original.pos.start)
    case OriginalSubTree(original)     => List(original.pos.start)
    case ApplyTree(function, args)     => (function :: args).flatMap(originsOf)
    case SelectTree(qualifier, _)      => originsOf(qualifier)
    case TypeApplyTree(function, _)    => originsOf(function)
    case FunctionTree(_, body)         => originsOf(body)
    case MacroExpansionTree(before, _) => originsOf(before)
    case _                             => Nil
  }

  // The symbols of the inserted code: every `IdTree` in a synthetic. The
  // original code inside it is skipped, since the names written there are
  // already mentions in their own right.
  private def symbolsIn(tree: SemanticTree): List[Symbol] = tree match {
    case id: IdTree                    => List(id.symbol)
    case SelectTree(qualifier, id)     => symbolsIn(qualifier) :+ id.symbol
    case ApplyTree(function, args)     => (function :: args).flatMap(symbolsIn)
    case TypeApplyTree(function, _)    => symbolsIn(function)
    case FunctionTree(_, body)         => symbolsIn(body)
    case MacroExpansionTree(before, _) => symbolsIn(before)
    case _                             => Nil
  }
}
