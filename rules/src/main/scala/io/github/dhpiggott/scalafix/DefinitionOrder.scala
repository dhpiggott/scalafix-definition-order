package io.github.dhpiggott.scalafix

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.seqOrdering
import scala.meta._

import metaconfig.ConfDecoder
import metaconfig.Configured
import metaconfig.generic.Surface
import scalafix.lint.Diagnostic
import scalafix.lint.LintSeverity
import scalafix.v1._

final case class DefinitionOrderConfig(exclude: List[String] = Nil)

object DefinitionOrderConfig {
  val default: DefinitionOrderConfig = DefinitionOrderConfig()

  // `surface` is metaconfig's description of the config's fields (their
  // names, types and defaults), which `deriveDecoder` reads
  // `DefinitionOrder.exclude` by. The macro takes it as an implicit argument
  // that SemanticDB doesn't record, so the rule can't see that `decoder` uses
  // it, and would put it below.
  // scalafix:off DefinitionOrder
  implicit val surface: Surface[DefinitionOrderConfig] =
    metaconfig.generic.deriveSurface[DefinitionOrderConfig]

  implicit val decoder: ConfDecoder[DefinitionOrderConfig] =
    metaconfig.generic.deriveDecoder(default)
  // scalafix:on DefinitionOrder
}

// Reported inside every kept range, where scalafix suppresses it. That is what
// tells scalafix the `scalafix:off` comment was needed, rather than warning
// that it suppressed nothing.
final case class KeptAsWritten(position: Position) extends Diagnostic {
  override def message: String =
    "Kept in its written order by a scalafix:off comment."
  override def severity: LintSeverity = LintSeverity.Info
}

final case class Misplaced(position: Position, message: String)
    extends Diagnostic

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

  override def fix(implicit doc: SemanticDocument): Patch = {
    val path = DefinitionOrder.pathOf(doc)
    if (config.exclude.exists(_.r.findFirstIn(path).isDefined)) Patch.empty
    else new DefinitionOrder.FileOrder().patch
  }

  override def withConfiguration(
      configuration: Configuration
  ): Configured[Rule] =
    configuration.conf
      .getOrElse("DefinitionOrder")(config)
      .map(new DefinitionOrder(_))
}

object DefinitionOrder {

  private final case class Occurrence(offset: Int, symbol: Symbol)

  // Offsets into the file, end exclusive.
  private final case class Span(start: Int, end: Int) {
    def contains(other: Span): Boolean =
      start <= other.start && other.end <= end

    def overlaps(other: Span): Boolean = start < other.end && other.start < end
  }

  private final case class Marker(isOff: Boolean, comment: Token.Comment)

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

  private final case class Item(
      stats: List[Stat],
      kept: Option[Span],
      key: SortKey,
      isType: Boolean,
      isStrict: Boolean,
      symbols: Set[Symbol]
  )

  // An item's index in its scope's `items`. Orders, edges and phases are made
  // of these; most of the other Ints are offsets into the file.
  private type ItemId = Int

  // Part of a run of definitions: one definition's block, or a whole kept
  // range's, along with the kept item it belongs to.
  // A scope's items, and the order they go in (their ids, top to bottom).
  private final case class Ordered(items: Vector[Item], order: List[ItemId])

  private final case class Edit(start: Int, end: Int, text: String)

  // Only a class, trait or enum body is `fieldsFirst`.
  private final case class Scope(stats: List[Stat], fieldsFirst: Boolean)

  private final case class Member(stat: Stat, names: List[Name])

  private final case class Mentions(used: List[ItemId], runs: List[ItemId])

  // The depth-first walk. `pulls` are what an item needs directly above it
  // (types it names, strict vals its initialiser reads), placed just before
  // it; `children` are what it uses, each placed once everything that uses it
  // outside its own cycle has been. `component(id)` numbers the cycle an item
  // is in (its strongly connected component), so two items with the same
  // number are in one cycle, and an item in no cycle has a number of its own.
  private final class Walk(
      items: Vector[Item],
      pulls: Vector[List[ItemId]],
      children: Vector[List[ItemId]],
      waits: Vector[Set[ItemId]],
      component: Vector[Int]
  ) {

    private val placed = Array.fill(items.size)(false)

    private val pulled = pulls.flatten.toSet

    private var reversed = List.empty[ItemId]

    // Anything the walk didn't reach from a root, such as the members of a
    // cycle no root leads into: ready ones first, then in key order, leaving
    // what something pulls for that to place.
    @tailrec def finish(phase: Set[ItemId]): Unit = {
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

    def visit(id: ItemId, phase: Set[ItemId]): Unit =
      if (!placed(id)) {
        val newlyPlaced = place(id, phase)
        val next = (id :: newlyPlaced.filter(_ != id)).flatMap(children(_))
        next.distinct.foreach { child =>
          if (phase(child) && isReady(child)) visit(child, phase)
        }
      }

    // A pull from another phase waits for its own: fields come first even
    // when one names a type defined among the rest.
    private def place(id: ItemId, phase: Set[ItemId]): List[ItemId] =
      if (placed(id)) Nil
      else {
        placed(id) = true
        val before = pulls(id).filter(phase).flatMap(place(_, phase))
        reversed = id :: reversed
        before :+ id
      }

    def isReady(id: ItemId): Boolean =
      waits(id).forall(user => placed(user) || component(user) == component(id))

    def isPulled(id: ItemId): Boolean = pulled(id)

    def order: List[ItemId] = reversed.reverse
  }

  private final case class InItem(position: Int) extends Place

  // One step of where an offset will end up, outermost first: at that offset
  // among what isn't an item (a definition's header, say, or anything inside
  // a kept range, which keeps its written order), or inside the item at a
  // position in its scope's settled order. A header comes before its body.
  private sealed trait Place

  private object Place {
    implicit val ordering: Ordering[Place] = Ordering.by {
      case InHeader(offset) => (0, offset)
      case InItem(position) => (1, position)
    }
  }

  private final case class InHeader(offset: Int) extends Place

  // `runs` is false where the item is only named as a type.
  private final case class Mention(item: ItemId, runs: Boolean)

  private final case class Piece(block: Span, keptItem: Option[ItemId])

  // A scope's definitions as written: the pieces, the span from the first to
  // the last, the indent the first starts at, and the kept pieces.
  private final case class Run(
      pieces: List[Piece],
      span: Span,
      indent: String,
      anchors: List[Piece]
  )

  // Why a scope can't be rewritten, and the part of the file to report it at.
  private final case class Obstacle(span: Span, message: String)

  private final class FileOrder(implicit doc: SemanticDocument) {

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
    // (`describe(1)`) inside it. Each symbol inserted counts as mentioned
    // where that code starts, so whichever definition holds the code
    // (`render`) is its user, and `defaultShow` goes below `render`. Without
    // this, a given used only implicitly would have no users at all. If a
    // synthetic holds more than one piece of original code, the first is
    // used: `originsOf` looks in an application's function before its
    // arguments.
    private val inferredUses: List[Occurrence] =
      doc.synthetics.toList.flatMap { synthetic =>
        originsOf(synthetic).headOption.toList.flatMap { origin =>
          symbolsIn(synthetic).map(Occurrence(origin, _))
        }
      }

    // Declared before `keptRanges` and `okLines`, whose initialisers read it.
    private val tokens: Vector[Token] = doc.tokens.toVector

    private val text: String = doc.input.text

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

    // Each scope's canonical order, keyed by the scope's own list of
    // statements: two scopes can be equal as trees, but never the same list.
    private val orders =
      new java.util.IdentityHashMap[List[Stat], Ordered]

    private var problems = List.empty[Patch]

    private val tokenStarts: Array[Int] = tokens.map(_.start).toArray

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
        else patches.flatMap { case (_, patch) => patch }.asPatch.atomic
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
      val members = membersOf(scope)
      val Ordered(items, order) = canonicalOrder(scope, members)
      items.filter(_.kept.isDefined).foreach { item =>
        problems = Patch.lint(KeptAsWritten(item.stats.head.pos)) :: problems
      }
      lazy val nestedEdits =
        items.filter(_.kept.isEmpty).flatMap(_.stats).flatMap(editsWithin)
      val canonical = order.flatMap(items(_).stats).map(_.pos.start)
      if (canonical == members.map(_.stat.pos.start)) nestedEdits.toList
      else reordered(items, order).getOrElse(nestedEdits.toList)
    }

    // Worked out once per scope: the scope around it reads it too (see
    // `settledKey`).
    private def canonicalOrder(
        scope: Scope,
        members: List[Member]
    ): Ordered =
      Option(orders.get(scope.stats)).getOrElse {
        val order = orderOf(scope, members)
        orders.put(scope.stats, order)
        order
      }

    private def orderOf(
        scope: Scope,
        members: List[Member]
    ): Ordered = {
      val items = itemsOf(members)
      // Which item defines each symbol, so a mention leads to the item used.
      val owners = items.indices
        .flatMap(id => items(id).symbols.map(_ -> id))
        .toMap
      val mentions =
        items.indices.toVector.map(mentionsBy(_, items, owners))
      val runs = mentions.map(_.runs)
      val isField = items.map(item => scope.fieldsFirst && item.isStrict)
      // Fields come first whatever uses them, so a field used by something
      // other than a field isn't placed below that user.
      val uses = items.indices.toVector.map { user =>
        mentions(user).used.filterNot(used => isField(used) && !isField(user))
      }
      // What has to sit directly above an item rather than below its users:
      // the types it names and, for a strict val, every strict val its
      // initialiser reads. Everything else it uses is a child, placed below.
      val pulls = items.indices.toVector.map { user =>
        val reads =
          if (items(user).isStrict) strictReads(user, items, runs) else Nil
        val candidates = (uses(user) ++ reads).distinct
        candidates.filter(used => items(used).isType || reads.contains(used))
      }
      val children = items.indices.toVector.map { user =>
        uses(user).filterNot(pulls(user).contains)
      }
      // An item waits for all its users to be placed, except those in its own
      // cycle, which would otherwise wait for each other for ever.
      val waits = reverse(children).map(_.toSet)
      val component = stronglyConnected(items.size, pulls, children)
      val walk = new Walk(items, pulls, children, waits, component)
      val phases =
        if (!scope.fieldsFirst) List(items.indices)
        else {
          val (fields, others) = items.indices.partition(isField)
          List(fields, others)
        }
      // The walk starts from every root (ready, and not waiting to be pulled
      // above something) in key order, then places whatever it didn't reach.
      phases.foreach { phase =>
        val ids = phase.toSet
        phase
          .filter(id => walk.isReady(id) && !walk.isPulled(id))
          .sortBy(items(_).key)
          .foreach(walk.visit(_, ids))
        walk.finish(ids)
      }
      Ordered(items, walk.order)
    }

    // Members grouped into items: everything in one kept range is one item
    // (keyed by a negative number, so it can't clash with an index), a class, trait
    // or enum shares its name with its companion, and anything else is an
    // item of its own. Items come in the order their first member was
    // written, and a class goes before its companion.
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
        .sortBy(group => group.map { case (_, index) => index }.min)
        .map { group =>
          val written =
            group.sortBy { case (_, index) => index }.map { case (member, _) =>
              member.stat
            }
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

    private def companionKey(member: Member, index: Int): Either[String, Int] =
      member.stat match {
        case _: Defn.Object | _: Defn.Class | _: Defn.Trait | _: Defn.Enum =>
          Left(member.names.head.value)
        case _ => Right(index)
      }

    private def hasSyntheticCompanion(stat: Stat): Boolean = stat match {
      case _: Defn.Class | _: Defn.Trait | _: Defn.Enum => true
      case _                                            => false
    }

    // A class, trait or enum with no companion written out still has one in
    // SemanticDB, and it is what `Type(...)` and `Type.Case` resolve to. A
    // type alias has none, and an object of the same name is its own item.
    private def withCompanion(symbol: Symbol): List[Symbol] =
      List(symbol, Symbol(symbol.value.stripSuffix("#") + "."))

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

    // The other items an item uses, in the order its body first mentions
    // them once the scopes nested in it are in order (see `settledKey`); and,
    // of those, the ones it mentions other than as a bare type.
    private def mentionsBy(
        id: ItemId,
        items: Vector[Item],
        owners: Map[Symbol, ItemId]
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
      val kept = items(id).kept.isDefined
      val mentioned = (named ++ inferred)
        .sortBy { occurrence =>
          if (kept) List(InHeader(occurrence.offset))
          else settledKey(stats, occurrence.offset)
        }
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

    // Where an offset among `stats` will be once every scope nested in them is
    // in the order this run of the rule writes, as a key that sorts in that
    // order. A scope reads the order its items' bodies first mention things
    // in; read from the bodies as written, that would change once the bodies
    // were rewritten, and the next run would move the scope again.
    private def settledKey(stats: List[Stat], offset: Int): List[Place] =
      stats.zipWithIndex
        .collectFirst {
          case (stat, index) if covers(stat, offset) =>
            InItem(index) :: keyWithin(stat, offset)
        }
        .getOrElse(List(InHeader(offset)))

    // Inside a definition, anything before its body (its header) comes first,
    // then its body's items in the order they will be in.
    private def keyWithin(stat: Stat, offset: Int): List[Place] =
      settledOrder(stat)
        .flatMap { case Ordered(items, order) =>
          order.zipWithIndex.collectFirst {
            case (id, position) if items(id).stats.exists(covers(_, offset)) =>
              val inside =
                if (items(id).kept.isDefined) List(InHeader(offset))
                else settledKey(items(id).stats, offset)
              InItem(position) :: inside
          }
        }
        .getOrElse(List(InHeader(offset)))

    // The order the scope nested in a definition will end up in: its
    // canonical order, unless the rule can't rewrite it, which leaves it as
    // written (None).
    private def settledOrder(stat: Stat): Option[Ordered] =
      nested(stat).flatMap { scope =>
        val members = membersOf(scope)
        val Ordered(items, order) = canonicalOrder(scope, members)
        val canonical = order.flatMap(items(_).stats).map(_.pos.start)
        val inOrder = canonical == members.map(_.stat.pos.start)
        if (inOrder || obstacle(items, order).isEmpty)
          Some(Ordered(items, order))
        else None
      }

    private def membersOf(scope: Scope): List[Member] =
      scope.stats.flatMap(stat => definedNames(stat).map(Member(stat, _)))

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

    private def patternNames(pat: Pat): List[Name] =
      pat.collect { case Pat.Var(name) => name }

    private def extensionNames(defn: Defn.ExtensionGroup): List[Name] =
      extensionMembers(defn).flatMap {
        case member: Defn.Def => List(member.name)
        case member: Decl.Def => List(member.name)
        case _                => Nil
      }

    private def covers(stat: Stat, offset: Int): Boolean =
      stat.pos.start <= offset && offset < stat.pos.end

    private def ownerIn(
        owners: Map[Symbol, ItemId],
        symbol: Symbol
    ): Option[ItemId] =
      Iterator
        .iterate(symbol)(_.owner)
        .takeWhile(s => !s.isNone && !s.isRootPackage && !s.isEmptyPackage)
        .flatMap(owners.get)
        .nextOption()

    // The strict vals a strict val's initialiser reads, directly or through
    // what it runs, all of which have to be initialised before it. `runs`
    // leaves out mentions that only name a type, which run nothing, but
    // follows the rest into a type: constructing a class runs its body, and
    // touching a companion initialises it.
    private def strictReads(
        start: Int,
        items: Vector[Item],
        runs: Vector[List[ItemId]]
    ): List[ItemId] = {
      @tailrec def loop(
          todo: List[ItemId],
          seen: Set[ItemId],
          found: List[ItemId]
      ): List[ItemId] = todo match {
        case Nil        => found.reverse
        case id :: rest =>
          if (seen(id) || id == start) loop(rest, seen, found)
          else if (items(id).isStrict) loop(rest, seen + id, id :: found)
          else loop(runs(id) ++ rest, seen + id, found)
      }
      loop(runs(start), Set.empty, Nil)
    }

    private def editsWithin(stat: Stat): List[Edit] =
      nested(stat).toList.flatMap(editsFor)

    private def nested(stat: Stat): Option[Scope] = {
      val stats = stat match {
        case defn: Defn.Object         => defn.templ.body.stats
        case defn: Defn.Class          => defn.templ.body.stats
        case defn: Defn.Trait          => defn.templ.body.stats
        case defn: Defn.Enum           => defn.templ.body.stats
        case defn: Defn.ExtensionGroup => extensionMembers(defn)
        case _                         => Nil
      }
      val fieldsFirst = stat match {
        case _: Defn.Class | _: Defn.Trait | _: Defn.Enum => true
        case _                                            => false
      }
      Some(Scope(stats, fieldsFirst = fieldsFirst)).filter(_.stats.nonEmpty)
    }

    // The whole run of definitions, rewritten in the canonical order with one
    // blank line between them, or None, having reported why, if it can't be.
    private def reordered(
        items: Vector[Item],
        order: List[ItemId]
    ): Option[List[Edit]] = {
      val Run(pieces, run, indent, anchors) = runOf(items)
      obstacle(items, order) match {
        case Some(Obstacle(span, message)) =>
          report(Position.Range(doc.input, span.start, span.end), message)
          None
        case None =>
          // The kept ranges stay put and cut the run into segments: before
          // the first, between each pair, and after the last. The canonical
          // order is cut at the same kept items, so segment n gets group n.
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
              // A segment already in order keeps its text, and only what
              // changes inside its definitions is edited.
              if (written == stats.map(blockOf(_).start))
                stats.flatMap(editsWithin)
              else {
                // The run's first and last segments border text outside it,
                // which has its own spacing; every other segment sits between
                // two kept ranges and needs a blank line on each side, or
                // just one between them if it's left empty.
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

    // Why a scope can't be rewritten into `order`, if it can't, and where.
    // Only whitespace may separate its definitions: anything else between two
    // of them has nowhere to go once they move.
    private def obstacle(
        items: Vector[Item],
        order: List[ItemId]
    ): Option[Obstacle] = {
      val Run(pieces, run, indent, anchors) = runOf(items)
      val blocks = pieces.map(_.block)
      val gaps = blocks.zip(blocks.drop(1)).map { case (before, after) =>
        Span(before.end, after.start)
      }
      val blocked = gaps.find { gap =>
        gap.start > gap.end || text.substring(gap.start, gap.end).trim.nonEmpty
      }
      blocked match {
        case Some(gap) =>
          Some(
            Obstacle(
              Span(gap.start, gap.end.max(gap.start)),
              "These definitions are out of order, but something other than " +
                "whitespace separates two of them, so they can't be moved " +
                "automatically. A comment moves with the definition directly " +
                "below it; one separated from it by a blank line doesn't."
            )
          )
        case None if indent.trim.nonEmpty =>
          Some(
            Obstacle(
              run,
              "These definitions are out of order, but they don't start " +
                "on a line of their own, so they can't be moved automatically."
            )
          )
        case None if suppressedWithin(run, anchors.map(_.block)) =>
          Some(
            Obstacle(
              run,
              "These definitions are out of order, but one that would " +
                "move holds a scalafix:off range or a scalafix:ok comment, and " +
                "scalafix won't edit inside those. Move them by hand."
            )
          )
        case None
            if anchors.flatMap(_.keptItem) !=
              order.filter(items(_).kept.nonEmpty) =>
          Some(
            Obstacle(
              run,
              "These definitions are out of order, and putting them in " +
                "order would move one scalafix:off range past another, which " +
                "the rule leaves in place. Move the ranges by hand."
            )
          )
        case None => None
      }
    }

    // A scope's definitions, laid out as written, as the pieces a rewrite
    // moves.
    private def runOf(items: Vector[Item]): Run = {
      val pieces = items.indices.toList
        .flatMap { id =>
          items(id).kept match {
            case Some(_) => List(Piece(keptBlockOf(items(id)), Some(id)))
            case None => items(id).stats.map(stat => Piece(blockOf(stat), None))
          }
        }
        .sortBy(_.block.start)
      val run = Span(pieces.head.block.start, pieces.last.block.end)
      Run(
        pieces = pieces,
        span = run,
        indent = text.substring(lineStart(run.start), run.start),
        anchors = pieces.filter(_.keptItem.isDefined)
      )
    }

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

    // Whether a suppressed range overlaps the part of the run the rule would
    // rewrite: all of the run except the anchors.
    private def suppressedWithin(run: Span, anchors: List[Span]): Boolean =
      (keptRanges ++ okLines).exists { range =>
        range.overlaps(run) && !anchors.exists(_.contains(range))
      }

    private def report(position: Position, message: String): Unit =
      problems = Patch.lint(Misplaced(position, message)) :: problems

    // The canonical order cut at each kept item, which stays where it is.
    private def splitAt(
        order: List[ItemId],
        items: Vector[Item]
    ): List[List[ItemId]] =
      order
        .foldLeft(List(List.empty[ItemId])) { (groups, id) =>
          if (items(id).kept.isDefined) Nil :: groups
          else (id :: groups.head) :: groups.tail
        }
        .map(_.reverse)
        .reverse

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

    // Walks up from a definition's first line over the comments directly
    // above it: `//` lines, and `/* */` blocks that start a line of their own.
    // A blank line or code stops it.
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

    // Scalafix patches whole tokens, so a replacement becomes the first token
    // it covers replaced with the new text and the rest removed. That only
    // works if the edit starts exactly on a token; None means it doesn't. An
    // edit covering nothing is an insertion.
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

    private def appliesHere(rules: List[String]): Boolean =
      rules.isEmpty || rules.contains("DefinitionOrder")
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

  // Tarjan's algorithm over both kinds of edge, each pointing from the item
  // that goes above to the one that goes below.
  private def stronglyConnected(
      size: Int,
      pulls: Vector[List[ItemId]],
      children: Vector[List[ItemId]]
  ): Vector[Int] = {
    val pulledBy = reverse(pulls)
    val below = Vector.tabulate(size)(id => children(id) ++ pulledBy(id))
    val index = Array.fill(size)(-1)
    val low = Array.fill(size)(0)
    val onStack = Array.fill(size)(false)
    val component = Array.fill(size)(0)
    var stack = List.empty[ItemId]
    var visited = 0
    var components = 0
    def visit(id: ItemId): Unit = {
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

  // For each item, the items whose edges point at it, in ascending order.
  private def reverse(edges: Vector[List[ItemId]]): Vector[List[ItemId]] = {
    val from = Array.fill(edges.size)(List.empty[ItemId])
    edges.indices.reverse.foreach { id =>
      edges(id).distinct.foreach(to => from(to) = id :: from(to))
    }
    from.toVector
  }

  private def extensionMembers(defn: Defn.ExtensionGroup): List[Stat] =
    defn.body match {
      case block: Term.Block => block.stats
      case stat              => List(stat)
    }

  private def pathOf(doc: SemanticDocument): String = {
    val path = doc.input match {
      case Input.File(file, _)        => file.toString
      case Input.VirtualFile(file, _) => file
      case other                      => other.syntax
    }
    path.replace('\\', '/')
  }
}
