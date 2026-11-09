package io.github.dhpiggott.scalafix

import scala.meta.*

import metaconfig.Configured
import scalafix.v1.*

// Puts the definitions of each scope in one canonical order, worked out from
// the code alone, so the order they start in never matters. A scope is a
// file's top level, or the body of one object, class, trait, enum or extension
// block. The README describes the order; this is the one implementation of it.
//
// A class and its companion are one item, class first. Within a scope:
//
//   - A term goes below everything that uses it, and is placed as soon as
//     everything that uses it has been: depth first from each root, visiting
//     what an item uses in the order its body first mentions it, with any
//     class or object body in it read in the order the rule puts it in.
//   - A type, and a strict val another strict val's initialiser reads, goes
//     immediately above the first item that needs it, except that a subtype
//     goes below the type it extends, even where that type names it.
//   - Roots, which nothing in the scope uses, come public before private and
//     then by name; so does anything a cycle leaves undecided.
//   - A class, trait or enum body puts its fields (strict vals and vars)
//     first, then everything else.
//
// The rule rewrites a scope that is out of order, so `scalafixAll` fixes it
// and `scalafixAll --check` fails on it. A comment directly above a definition
// moves with it; a scope it can't rewrite safely, such as one with a comment
// standing apart from any definition, is reported instead.
//
// Definitions between `// scalafix:off DefinitionOrder` and
// `// scalafix:on DefinitionOrder` keep their written order, as one item that
// the rest of the scope is ordered around. Scalafix drops any patch touching
// that range, so the rule never edits inside it, only on either side.
//
// The implementation is in layers, one file each:
//
//   - `Scopes`: the scopes of a file and the items they're ordered as, read
//     from the tree alone.
//   - `Synthetics`: the uses the compiler fills in, read from SemanticDB.
//   - `CanonicalOrder` and `Walk`: the order each scope's items go in.
//   - `SourceText`: the file as text, meaning where each definition and its
//     comments start and end, what mustn't be edited, and whether a scope
//     can be rewritten.
//   - `FileOrder`: the rewrite itself, and what is reported instead.
class DefinitionOrder(config: DefinitionOrderConfig)
    extends SemanticRule("DefinitionOrder") {

  def this() = this(DefinitionOrderConfig.default)

  override def fix(implicit doc: SemanticDocument): Patch = {
    val path = DefinitionOrder.pathOf(doc)
    if (config.exclude.exists(_.r.findFirstIn(path).isDefined)) Patch.empty
    else new FileOrder().patch
  }

  override def withConfiguration(
      configuration: Configuration
  ): Configured[Rule] =
    configuration.conf
      .getOrElse("DefinitionOrder")(config)
      .map(new DefinitionOrder(_))
}

object DefinitionOrder {
  private[scalafix] def pathOf(doc: SemanticDocument): String = {
    val path = doc.input match {
      case Input.File(file, _)        => file.toString
      case Input.VirtualFile(file, _) => file
      case other                      => other.syntax
    }
    path.replace('\\', '/')
  }
}
