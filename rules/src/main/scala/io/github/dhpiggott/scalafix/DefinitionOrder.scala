package io.github.dhpiggott.scalafix

import scala.annotation.tailrec
import scala.meta._

import metaconfig.ConfDecoder
import metaconfig.Configured
import metaconfig.generic.Surface
import scalafix.lint.Diagnostic
import scalafix.lint.LintSeverity
import scalafix.v1._

// Puts the definitions of each scope in one canonical order, worked out from
// the code alone, so the order they start in never matters. A scope is a
// file's top level, or the body of one object, class, trait, enum or extension
// block. The README describes the order; this is the one implementation of it.
//
// A class and its companion are one item, class first. Within a scope:
//
//   - A term goes below everything that uses it, and is placed as soon as
//     everything that uses it has been: depth first from each root, visiting
//     what an item uses in the order its body first mentions it.
//   - A type, and a strict val another strict val's initialiser reads, goes
//     immediately above the first item that needs it.
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
class DefinitionOrder(config: DefinitionOrderConfig)
    extends SemanticRule("DefinitionOrder") {

  def this() = this(DefinitionOrderConfig.default)

  override def withConfiguration(
      configuration: Configuration
  ): Configured[Rule] =
    configuration.conf
      .getOrElse("DefinitionOrder")(config)
      .map(new DefinitionOrder(_))

  override def fix(implicit doc: SemanticDocument): Patch = {
    val path = DefinitionOrder.pathOf(doc)
    if (config.exclude.exists(_.r.findFirstIn(path).isDefined)) Patch.empty
    else new DefinitionOrder.FileOrder().patch
  }
}

object DefinitionOrder {

  private def pathOf(doc: SemanticDocument): String = {
    val path = doc.input match {
      case Input.File(file, _)        => file.toString
      case Input.VirtualFile(file, _) => file
      case other                      => other.syntax
    }
    path.replace('\\', '/')
  }

  private final case class Scope(stats: List[Stat], fieldsFirst: Boolean)

  private final case class Member(stat: Stat, names: List[Name])

  private final case class Edit(start: Int, end: Int, text: String)

  // Offsets into the file, end exclusive.
  private final case class Span(start: Int, end: Int) {
    def overlaps(other: Span): Boolean = start < other.end && other.start < end
    def contains(other: Span): Boolean =
      start <= other.start && other.end <= end
  }

  private final case class Item(
      stats: List[Stat],
      kept: Option[Span],
      key: SortKey,
      isType: Boolean,
      isStrict: Boolean,
      symbols: Set[Symbol]
  )

  // How whatever the uses leave open is ordered: public before private, then
  // by name, then by source text, which tells overloads apart.
  private final case class SortKey(
      isPrivate: Boolean,
      name: String,
      source: String
  )

  private object SortKey {
    implicit val ordering: Ordering[SortKey] =
      Ordering.by(key => (key.isPrivate, key.name, key.source))
  }

  private final case class Marker(isOff: Boolean, comment: Token.Comment)

  private final case class Occurrence(offset: Int, symbol: Symbol)

  // `runs` is false where the item is only named as a type.
  private final case class Mention(item: Int, runs: Boolean)

  private final case class Mentions(used: List[Int], runs: List[Int])

  // Part of a run of definitions: one definition's block, or a whole kept
  // range's, along with the kept item it belongs to.
  private final case class Piece(block: Span, keptItem: Option[Int])

  private final class FileOrder(implicit doc: SemanticDocument) {

    private val text: String = doc.input.text

    private var problems = List.empty[Patch]

    // Declared before `keptRanges` and `okLines`, whose initialisers read it.
    private val tokens: Vector[Token] = doc.tokens.toVector

    // The ranges a `scalafix:off` comment disables this rule for, from that
    // comment to the end of the `scalafix:on` that ends it, or to the end of
    // the file if nothing does. A `scalafix:on` with no range open, or a
    // `scalafix:off` inside one, changes nothing; scalafix reports both as
    // unused suppressions.
    private val keptRanges: List[Span] = {
      val markers = tokens.collect { case comment: Token.Comment =>
        wordsOf(comment) match {
          case "scalafix:off" :: rules if appliesHere(rules) =>
            List(Marker(isOff = true, comment))
          case "scalafix:on" :: rules if appliesHere(rules) =>
            List(Marker(isOff = false, comment))
          case _ => Nil
        }
      }.flatten
      val (ranges, open) = markers.foldLeft(
        (List.empty[Span], Option.empty[Int])
      ) {
        case ((ranges, None), Marker(true, off)) => (ranges, Some(off.start))
        case ((ranges, Some(start)), Marker(false, on)) =>
          (Span(start, on.end) :: ranges, None)
        case (state, _) => state
      }
      (open.map(Span(_, text.length)).toList ++ ranges).reverse
    }

    // The lines a `scalafix:ok` comment disables this rule on.
    private val okLines: List[Span] =
      tokens.collect {
        case comment: Token.Comment if isOkComment(comment) =>
          Span(lineStart(comment.start), comment.end)
      }.toList

    private val tokenStarts: Array[Int] = tokens.map(_.start).toArray

    // What the compiler filled in, such as a given passed as a context
    // argument, at the offset of the code it was filled in for. Without these
    // a given used only implicitly would have no users at all.
    private val inferredUses: List[Occurrence] =
      doc.synthetics.toList.flatMap { synthetic =>
        originsOf(synthetic).headOption.toList.flatMap { origin =>
          symbolsIn(synthetic).map(Occurrence(origin, _))
        }
      }

    private def appliesHere(rules: List[String]): Boolean =
      rules.isEmpty || rules.contains("DefinitionOrder")

    private def isOkComment(comment: Token.Comment): Boolean =
      wordsOf(comment) match {
        case "scalafix:ok" :: rules => appliesHere(rules)
        case _                      => false
      }

    // A comment's words, split on runs of whitespace and commas, so that
    // `// scalafix:off DefinitionOrder, Other` reads as `scalafix:off`,
    // `DefinitionOrder` and `Other`.
    private def wordsOf(comment: Token.Comment): List[String] =
      comment.value.trim.split("[\\s,]+").toList

    // The moves are one atomic patch: each edit only makes sense with all the
    // others, so if scalafix would drop any part of one, it drops them all.
    def patch: Patch = {
      val edits = scopes(doc.tree).flatMap(editsFor)
      val patches = edits.map(edit => (edit, toPatch(edit)))
      val unplaceable = patches.collect { case (edit, None) => edit }
      unplaceable.foreach { edit =>
        report(
          Position.Range(doc.input, edit.start, edit.end),
          "These definitions are out of order, but there is nowhere beside " +
            "this point to write them, so they can't be moved automatically."
        )
      }
      val moves =
        if (unplaceable.nonEmpty) Patch.empty
        else patches.flatMap(_._2).asPatch.atomic
      (moves :: problems).asPatch
    }

    private def scopes(tree: Tree): List[Scope] = tree match {
      case source: Source => scopesIn(source.stats)
      case pkg: Pkg       => scopesIn(pkg.body.stats)
      case _              => Nil
    }

    private def scopesIn(stats: List[Stat]): List[Scope] = {
      val (packages, members) = stats.partition(_.isInstanceOf[Pkg])
      Scope(members, fieldsFirst = false) :: packages.flatMap(scopes)
    }

    private def editsFor(scope: Scope): List[Edit] = {
      val members = scope.stats.flatMap { stat =>
        definedNames(stat).map(Member(stat, _))
      }
      val (items, order) = canonicalOrder(scope, members)
      items.filter(_.kept.isDefined).foreach { item =>
        problems = Patch.lint(KeptAsWritten(item.stats.head.pos)) :: problems
      }
      lazy val nestedEdits =
        items.filter(_.kept.isEmpty).flatMap(_.stats).flatMap(editsWithin)
      val canonical = order.flatMap(items(_).stats).map(_.pos.start)
      if (canonical == members.map(_.stat.pos.start)) nestedEdits.toList
      else reordered(items, order).getOrElse(nestedEdits.toList)
    }

    private def editsWithin(stat: Stat): List[Edit] =
      nested(stat).toList.flatMap(editsFor)

    private def nested(stat: Stat): Option[Scope] = {
      val scope = stat match {
        case defn: Defn.Object         => Scope(defn.templ.body.stats, false)
        case defn: Defn.Class          => Scope(defn.templ.body.stats, true)
        case defn: Defn.Trait          => Scope(defn.templ.body.stats, true)
        case defn: Defn.Enum           => Scope(defn.templ.body.stats, true)
        case defn: Defn.ExtensionGroup => Scope(extensionMembers(defn), false)
        case _                         => Scope(Nil, false)
      }
      Some(scope).filter(_.stats.nonEmpty)
    }

    // The whole run of definitions, rewritten in the canonical order with one
    // blank line between them. Only whitespace may separate them: anything
    // else between two definitions has nowhere to go once they move. A kept
    // range stays exactly where it is, and what falls between kept ranges is
    // rewritten around them.
    private def reordered(
        items: Vector[Item],
        order: List[Int]
    ): Option[List[Edit]] = {
      val pieces = items.indices.toList
        .flatMap { id =>
          items(id).kept match {
            case Some(_) => List(Piece(keptBlockOf(items(id)), Some(id)))
            case None => items(id).stats.map(stat => Piece(blockOf(stat), None))
          }
        }
        .sortBy(_.block.start)
      val blocks = pieces.map(_.block)
      val gaps = blocks.zip(blocks.drop(1)).map { case (before, after) =>
        Span(before.end, after.start)
      }
      val run = Span(blocks.head.start, blocks.last.end)
      val indent = text.substring(lineStart(run.start), run.start)
      val anchors = pieces.filter(_.keptItem.isDefined)
      val blocked = gaps.find { gap =>
        gap.start > gap.end || text.substring(gap.start, gap.end).trim.nonEmpty
      }
      blocked match {
        case Some(gap) =>
          report(
            Position.Range(doc.input, gap.start, gap.end.max(gap.start)),
            "These definitions are out of order, but something other than " +
              "whitespace separates two of them, so they can't be moved " +
              "automatically. A comment moves with the definition directly " +
              "below it; one separated from it by a blank line doesn't."
          )
          None
        case None if indent.trim.nonEmpty =>
          report(
            Position.Range(doc.input, run.start, run.end),
            "These definitions are out of order, but they don't start on a " +
              "line of their own, so they can't be moved automatically."
          )
          None
        case None if suppressedWithin(run, anchors.map(_.block)) =>
          report(
            Position.Range(doc.input, run.start, run.end),
            "These definitions are out of order, but one that would move " +
              "holds a scalafix:off range or a scalafix:ok comment, and " +
              "scalafix won't edit inside those. Move them by hand."
          )
          None
        case None
            if anchors.flatMap(_.keptItem) !=
              order.filter(items(_).kept.nonEmpty) =>
          report(
            Position.Range(doc.input, run.start, run.end),
            "These definitions are out of order, and putting them in order " +
              "would move one scalafix:off range past another, which the " +
              "rule leaves in place. Move the ranges by hand."
          )
          None
        case None =>
          val separator = "\n\n" + indent
          val bounds = run.start :: anchors.flatMap { anchor =>
            List(anchor.block.start, anchor.block.end)
          } ++ List(run.end)
          val segments = bounds.grouped(2).toList.collect {
            case List(from, to) => Span(from, to)
          }
          val groups = splitAt(order, items).map(_.flatMap(items(_).stats))
          val last = segments.size - 1
          Some(segments.zip(groups).zipWithIndex.flatMap {
            case ((segment, stats), index) =>
              val written = pieces.collect {
                case Piece(block, None)
                    if segment.start <= block.start &&
                      block.start < segment.end =>
                  block.start
              }
              if (written == stats.map(blockOf(_).start))
                stats.flatMap(editsWithin)
              else {
                val body = stats.map(render).mkString(separator)
                val rendered =
                  if (body.isEmpty) {
                    if (index == 0 || index == last) "" else separator
                  } else
                    (if (index == 0) "" else separator) + body +
                      (if (index == last) "" else separator)
                List(Edit(segment.start, segment.end, rendered))
              }
          })
      }
    }

    // Whether a suppressed range overlaps the part of the run the rule would
    // rewrite: all of the run except the anchors.
    private def suppressedWithin(run: Span, anchors: List[Span]): Boolean =
      (keptRanges ++ okLines).exists { range =>
        range.overlaps(run) && !anchors.exists(_.contains(range))
      }

    // The canonical order cut at each kept item, which stays where it is.
    private def splitAt(
        order: List[Int],
        items: Vector[Item]
    ): List[List[Int]] =
      order
        .foldLeft(List(List.empty[Int])) { (groups, id) =>
          if (items(id).kept.isDefined) Nil :: groups
          else (id :: groups.head) :: groups.tail
        }
        .map(_.reverse)
        .reverse

    // A kept item's block runs from its `scalafix:off` comment to the end of
    // its `scalafix:on` one, when those sit directly against it, so that
    // nothing the rule writes lands inside the range.
    private def keptBlockOf(item: Item): Span = {
      val range = item.kept.getOrElse(Span(0, 0))
      val first = blockOf(item.stats.head).start
      val last = blockOf(item.stats.last).end
      val start =
        if (range.start < first && onlyComments(range.start, first))
          range.start
        else first
      val end =
        if (last < range.end && onlyComments(last, range.end)) range.end
        else last
      Span(start, end)
    }

    private def onlyComments(from: Int, to: Int): Boolean =
      text.substring(from, to).split('\n').forall { line =>
        line.trim.isEmpty || line.trim.startsWith("//")
      }

    private def report(position: Position, message: String): Unit =
      problems = Patch.lint(Misplaced(position, message)) :: problems

    private def render(stat: Stat): String = {
      val block = blockOf(stat)
      editsWithin(stat)
        .sortBy(-_.start)
        .foldLeft(text.substring(block.start, block.end)) { (rendered, edit) =>
          rendered.patch(
            edit.start - block.start,
            edit.text,
            edit.end - edit.start
          )
        }
    }

    // A definition together with the comments directly above it, and one
    // trailing it on its last line.
    private def blockOf(stat: Stat): Span = {
      val lineEnd = text.indexOf('\n', stat.pos.end) match {
        case -1  => text.length
        case end => end
      }
      val trailing = text.substring(stat.pos.end, lineEnd).trim
      val end = if (trailing.startsWith("//")) lineEnd else stat.pos.end
      Span(leadingStart(stat.pos.start), end)
    }

    @tailrec private def leadingStart(start: Int): Int = {
      val currentLine = lineStart(start)
      if (currentLine == 0 || text.substring(currentLine, start).trim.nonEmpty)
        start
      else {
        val previousEnd = currentLine - 1
        val previousStart = lineStart(previousEnd)
        val previous = text.substring(previousStart, previousEnd).trim
        val open = text.lastIndexOf("/*", previousEnd)
        val opensLine =
          open >= 0 && text.substring(lineStart(open), open).trim.isEmpty
        if (previous.startsWith("//"))
          leadingStart(text.indexOf("//", previousStart))
        else if (previous.endsWith("*/") && opensLine) leadingStart(open)
        else start
      }
    }

    private def lineStart(offset: Int): Int =
      text.lastIndexOf('\n', offset - 1) + 1

    private def toPatch(edit: Edit): Option[Patch] =
      if (edit.start == edit.end) insertAt(edit.start, edit.text)
      else {
        val inside = tokens
          .drop(firstTokenFrom(edit.start))
          .takeWhile(_.end <= edit.end)
          .toList
        inside.headOption.filter(_.start == edit.start).map { first =>
          Patch.removeTokens(inside.tail) + Patch.replaceToken(first, edit.text)
        }
      }

    // Beside whitespace rather than on the definition there, which may be
    // the first token of a kept range.
    private def insertAt(offset: Int, insertion: String): Option[Patch] = {
      val index = firstTokenFrom(offset)
      val nearby = tokens.slice(index - 2, index + 2)
      val whitespace = nearby.filter(_.isInstanceOf[Token.Whitespace])
      val before = whitespace.find(_.end == offset)
      val after = whitespace.find(_.start == offset)
      before
        .map(Patch.addRight(_, insertion))
        .orElse(after.map(Patch.addLeft(_, insertion)))
    }

    // The index of the first token starting at or after `offset`.
    private def firstTokenFrom(offset: Int): Int = {
      @tailrec def search(low: Int, high: Int): Int =
        if (low >= high) low
        else {
          val middle = (low + high) >>> 1
          if (tokenStarts(middle) < offset) search(middle + 1, high)
          else search(low, middle)
        }
      search(0, tokenStarts.length)
    }

    private def canonicalOrder(
        scope: Scope,
        members: List[Member]
    ): (Vector[Item], List[Int]) = {
      val items = itemsOf(members)
      val owners = items.indices
        .flatMap(id => items(id).symbols.map(_ -> id))
        .toMap
      val mentions =
        items.indices.toVector.map(mentionsBy(_, items, owners))
      val runs = mentions.map(_.runs)
      val isField = items.map(item => scope.fieldsFirst && item.isStrict)
      val uses = items.indices.toVector.map { user =>
        mentions(user).used.filterNot(used => isField(used) && !isField(user))
      }
      val pulls = items.indices.toVector.map { user =>
        val reads =
          if (items(user).isStrict) strictReads(user, items, runs) else Nil
        val candidates = (uses(user) ++ reads).distinct
        candidates.filter(used => items(used).isType || reads.contains(used))
      }
      val children = items.indices.toVector.map { user =>
        uses(user).filterNot(pulls(user).contains)
      }
      val waits = reverse(children).map(_.toSet)
      val component = stronglyConnected(items.size, pulls, children)
      val walk = new Walk(items, pulls, children, waits, component)
      val phases =
        if (!scope.fieldsFirst) List(items.indices)
        else {
          val (fields, others) = items.indices.partition(isField)
          List(fields, others)
        }
      phases.foreach { phase =>
        val ids = phase.toSet
        phase
          .filter(id => walk.isReady(id) && !walk.isPulled(id))
          .sortBy(items(_).key)
          .foreach(walk.visit(_, ids))
        walk.finish(ids)
      }
      (items, walk.order)
    }

    // The strict vals a strict val's initialiser reads, directly or through
    // what it runs, all of which have to be initialised before it. `runs`
    // leaves out mentions that only name a type, which run nothing, but
    // follows the rest into a type: constructing a class runs its body, and
    // touching a companion initialises it.
    private def strictReads(
        start: Int,
        items: Vector[Item],
        runs: Vector[List[Int]]
    ): List[Int] = {
      @tailrec def loop(
          todo: List[Int],
          seen: Set[Int],
          found: List[Int]
      ): List[Int] = todo match {
        case Nil        => found.reverse
        case id :: rest =>
          if (seen(id) || id == start) loop(rest, seen, found)
          else if (items(id).isStrict) loop(rest, seen + id, id :: found)
          else loop(runs(id) ++ rest, seen + id, found)
      }
      loop(runs(start), Set.empty, Nil)
    }

    // A class and its companion are one item, class first.
    private def itemsOf(members: List[Member]): Vector[Item] =
      members.zipWithIndex
        .groupBy { case (member, index) =>
          keptRangeOf(member.stat) match {
            case Some(range) => Right(-1 - keptRanges.indexOf(range))
            case None        => companionKey(member, index)
          }
        }
        .values
        .toVector
        .sortBy(_.map(_._2).min)
        .map { group =>
          val written =
            group.sortBy(_._2).map { case (member, _) => member.stat }
          val kept = keptRangeOf(written.head)
          val stats =
            if (kept.isDefined) written
            else
              written
                .sortBy(stat => if (stat.isInstanceOf[Defn.Object]) 1 else 0)
          val names = group.flatMap { case (member, _) => member.names }
          val symbols = group.flatMap { case (member, _) =>
            member.names.flatMap { name =>
              if (hasSyntheticCompanion(member.stat)) withCompanion(name.symbol)
              else List(name.symbol)
            }
          }
          val lead = stats.head
          Item(
            stats = stats,
            kept = kept,
            key = SortKey(
              isPrivate = isPrivate(lead),
              name = names.head.value,
              source = text.substring(lead.pos.start, lead.pos.end)
            ),
            isType = stats.exists(isType),
            isStrict = stats.exists(isStrict),
            symbols = symbols.toSet
          )
        }

    private def keptRangeOf(stat: Stat): Option[Span] =
      keptRanges.find(_.contains(Span(stat.pos.start, stat.pos.end)))

    // The other items an item uses, in the order its body first mentions
    // them; and, of those, the ones it mentions other than as a bare type.
    private def mentionsBy(
        id: Int,
        items: Vector[Item],
        owners: Map[Symbol, Int]
    ): Mentions = {
      val stats = items(id).stats
      val named = stats
        .flatMap(_.collect { case name: Name => name })
        .map(name => Occurrence(name.pos.start, name.symbol))
      val inferred = inferredUses.filter { use =>
        stats.exists { stat =>
          stat.pos.start <= use.offset && use.offset < stat.pos.end
        }
      }
      val mentioned = (named ++ inferred)
        .sortBy(_.offset)
        .flatMap { occurrence =>
          val runs = !occurrence.symbol.value.endsWith("#")
          ownerIn(owners, occurrence.symbol).map(Mention(_, runs))
        }
        .filter(_.item != id)
      Mentions(
        used = mentioned.map(_.item).distinct,
        runs = mentioned.filter(_.runs).map(_.item).distinct
      )
    }

    private def ownerIn(owners: Map[Symbol, Int], symbol: Symbol): Option[Int] =
      Iterator
        .iterate(symbol)(_.owner)
        .takeWhile(s => !s.isNone && !s.isRootPackage && !s.isEmptyPackage)
        .flatMap(owners.get)
        .nextOption()

    // A class, trait or enum with no companion written out still has one in
    // SemanticDB, and it is what `Type(...)` and `Type.Case` resolve to. A
    // type alias has none, and an object of the same name is its own item.
    private def withCompanion(symbol: Symbol): List[Symbol] =
      List(symbol, Symbol(symbol.value.stripSuffix("#") + "."))

    private def hasSyntheticCompanion(stat: Stat): Boolean = stat match {
      case _: Defn.Class | _: Defn.Trait | _: Defn.Enum => true
      case _                                            => false
    }

    private def companionKey(member: Member, index: Int): Either[String, Int] =
      member.stat match {
        case _: Defn.Object | _: Defn.Class | _: Defn.Trait | _: Defn.Enum =>
          Left(member.names.head.value)
        case _ => Right(index)
      }

    private def isType(stat: Stat): Boolean = stat match {
      case _: Defn.Class | _: Defn.Trait | _: Defn.Enum => true
      case _: Defn.Type | _: Decl.Type                  => true
      case _                                            => false
    }

    private def isStrict(stat: Stat): Boolean = stat match {
      case defn: Defn.Val => !defn.mods.exists(_.isInstanceOf[Mod.Lazy])
      case _: Defn.Var    => true
      case _              => false
    }

    private def isPrivate(stat: Stat): Boolean =
      modsOf(stat).exists {
        case _: Mod.Private | _: Mod.Protected => true
        case _                                 => false
      }

    private def modsOf(stat: Stat): List[Mod] = stat match {
      case defn: Defn.Def        => defn.mods
      case defn: Defn.Val        => defn.mods
      case defn: Defn.Var        => defn.mods
      case defn: Defn.Type       => defn.mods
      case defn: Defn.Class      => defn.mods
      case defn: Defn.Trait      => defn.mods
      case defn: Defn.Object     => defn.mods
      case defn: Defn.Enum       => defn.mods
      case defn: Defn.Given      => defn.mods
      case defn: Defn.GivenAlias => defn.mods
      case decl: Decl.Def        => decl.mods
      case decl: Decl.Val        => decl.mods
      case decl: Decl.Type       => decl.mods
      case _                     => Nil
    }

    private def definedNames(stat: Stat): Option[List[Name]] = {
      val names = stat match {
        case defn: Defn.Def            => List(defn.name)
        case defn: Defn.Val            => defn.pats.flatMap(patternNames)
        case defn: Defn.Var            => defn.pats.flatMap(patternNames)
        case defn: Defn.Type           => List(defn.name)
        case defn: Defn.Class          => List(defn.name)
        case defn: Defn.Trait          => List(defn.name)
        case defn: Defn.Object         => List(defn.name)
        case defn: Defn.Enum           => List(defn.name)
        case defn: Defn.Given          => List(defn.name)
        case defn: Defn.GivenAlias     => List(defn.name)
        case defn: Defn.ExtensionGroup => extensionNames(defn)
        case decl: Decl.Def            => List(decl.name)
        case decl: Decl.Val            => decl.pats.flatMap(patternNames)
        case decl: Decl.Type           => List(decl.name)
        case _                         => Nil
      }
      Some(names.filter(_.value.nonEmpty)).filter(_.nonEmpty)
    }

    private def extensionNames(defn: Defn.ExtensionGroup): List[Name] =
      extensionMembers(defn).flatMap {
        case member: Defn.Def => List(member.name)
        case member: Decl.Def => List(member.name)
        case _                => Nil
      }

    private def patternNames(pat: Pat): List[Name] =
      pat.collect { case Pat.Var(name) => name }
  }

  // Where in the source a synthetic was inferred, from the original code it
  // wraps.
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

  private def symbolsIn(tree: SemanticTree): List[Symbol] = tree match {
    case id: IdTree                    => List(id.symbol)
    case SelectTree(qualifier, id)     => symbolsIn(qualifier) :+ id.symbol
    case ApplyTree(function, args)     => (function :: args).flatMap(symbolsIn)
    case TypeApplyTree(function, _)    => symbolsIn(function)
    case FunctionTree(_, body)         => symbolsIn(body)
    case MacroExpansionTree(before, _) => symbolsIn(before)
    case _                             => Nil
  }

  private def extensionMembers(defn: Defn.ExtensionGroup): List[Stat] =
    defn.body match {
      case block: Term.Block => block.stats
      case stat              => List(stat)
    }

  // The depth-first walk. `pulls` are what an item needs directly above it
  // (types it names, strict vals its initialiser reads), placed just before
  // it; `children` are what it uses, each placed once everything that uses it
  // outside its own cycle has been.
  private final class Walk(
      items: Vector[Item],
      pulls: Vector[List[Int]],
      children: Vector[List[Int]],
      waits: Vector[Set[Int]],
      component: Vector[Int]
  ) {

    private val placed = Array.fill(items.size)(false)

    private val pulled = pulls.flatten.toSet

    private var reversed = List.empty[Int]

    def order: List[Int] = reversed.reverse

    def isPulled(id: Int): Boolean = pulled(id)

    def isReady(id: Int): Boolean =
      waits(id).forall(user => placed(user) || component(user) == component(id))

    def visit(id: Int, phase: Set[Int]): Unit =
      if (!placed(id)) {
        val newlyPlaced = place(id, phase)
        val next = (id :: newlyPlaced.filter(_ != id)).flatMap(children(_))
        next.distinct.foreach { child =>
          if (phase(child) && isReady(child)) visit(child, phase)
        }
      }

    // Anything the walk didn't reach from a root, such as the members of a
    // cycle no root leads into: ready ones first, then in key order, leaving
    // what something pulls for that to place.
    @tailrec def finish(phase: Set[Int]): Unit = {
      val remaining = phase.filterNot(placed(_)).toList.sortBy(items(_).key)
      val candidates = remaining.filterNot(pulled)
      val next = candidates
        .find(isReady)
        .orElse(candidates.headOption)
        .orElse(remaining.headOption)
      next match {
        case Some(id) =>
          visit(id, phase)
          finish(phase)
        case None => ()
      }
    }

    // A pull from another phase waits for its own: fields come first even
    // when one names a type defined among the rest.
    private def place(id: Int, phase: Set[Int]): List[Int] =
      if (placed(id)) Nil
      else {
        placed(id) = true
        val before = pulls(id).filter(phase).flatMap(place(_, phase))
        reversed = id :: reversed
        before :+ id
      }
  }

  // For each item, the items whose edges point at it, in ascending order.
  private def reverse(edges: Vector[List[Int]]): Vector[List[Int]] = {
    val from = Array.fill(edges.size)(List.empty[Int])
    edges.indices.reverse.foreach { id =>
      edges(id).distinct.foreach(to => from(to) = id :: from(to))
    }
    from.toVector
  }

  // Tarjan's algorithm over both kinds of edge, each pointing from the item
  // that goes above to the one that goes below.
  private def stronglyConnected(
      size: Int,
      pulls: Vector[List[Int]],
      children: Vector[List[Int]]
  ): Vector[Int] = {
    val pulledBy = reverse(pulls)
    val below = Vector.tabulate(size)(id => children(id) ++ pulledBy(id))
    val index = Array.fill(size)(-1)
    val low = Array.fill(size)(0)
    val onStack = Array.fill(size)(false)
    val component = Array.fill(size)(0)
    var stack = List.empty[Int]
    var visited = 0
    var components = 0
    def visit(id: Int): Unit = {
      index(id) = visited
      low(id) = visited
      visited += 1
      stack = id :: stack
      onStack(id) = true
      below(id).foreach { next =>
        if (index(next) < 0) {
          visit(next)
          low(id) = low(id).min(low(next))
        } else if (onStack(next)) low(id) = low(id).min(index(next))
      }
      if (low(id) == index(id)) {
        var member = -1
        while (member != id) {
          member = stack.head
          stack = stack.tail
          onStack(member) = false
          component(member) = components
        }
        components += 1
      }
    }
    (0 until size).foreach(id => if (index(id) < 0) visit(id))
    component.toVector
  }
}

final case class DefinitionOrderConfig(exclude: List[String] = Nil)

object DefinitionOrderConfig {
  val default: DefinitionOrderConfig = DefinitionOrderConfig()
  implicit val surface: Surface[DefinitionOrderConfig] =
    metaconfig.generic.deriveSurface[DefinitionOrderConfig]
  implicit val decoder: ConfDecoder[DefinitionOrderConfig] =
    metaconfig.generic.deriveDecoder(default)
}

final case class Misplaced(position: Position, message: String)
    extends Diagnostic

// Reported inside every kept range, where scalafix suppresses it. That is what
// tells scalafix the `scalafix:off` comment was needed, rather than warning
// that it suppressed nothing.
final case class KeptAsWritten(position: Position) extends Diagnostic {
  override def message: String =
    "Kept in its written order by a scalafix:off comment."
  override def severity: LintSeverity = LintSeverity.Info
}
