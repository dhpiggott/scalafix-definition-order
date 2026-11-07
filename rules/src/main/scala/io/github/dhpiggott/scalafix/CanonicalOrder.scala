package io.github.dhpiggott.scalafix

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.*
import scala.meta.Defn
import scala.meta.Name
import scala.meta.Stat

import io.github.dhpiggott.scalafix.CanonicalOrder.*
import io.github.dhpiggott.scalafix.Scopes.*
import io.github.dhpiggott.scalafix.Synthetics.Occurrence
import scalafix.v1.*

// Works out the order each scope's definitions go in: groups them into items,
// finds what each item uses, and walks those uses (see `Walk`).
private[scalafix] final class CanonicalOrder(source: SourceText)(implicit
    doc: SemanticDocument
) {

  private val inferredUses: List[Occurrence] = Synthetics.inferredUses(doc)

  // Each scope's canonical order, keyed by the scope's own list of
  // statements: two scopes can be equal as trees, but never the same list.
  private val orders =
    new java.util.IdentityHashMap[List[Stat], Ordered]

  // Worked out once per scope: the scope around it reads it too (see
  // `settledKey`).
  def apply(scope: Scope, members: List[Member]): Ordered =
    Option(orders.get(scope.stats)).getOrElse {
      val order = orderOf(scope, members)
      orders.put(scope.stats, order)
      order
    }

  private def orderOf(scope: Scope, members: List[Member]): Ordered = {
    val items = itemsOf(members)
    // Which item defines each symbol, so a mention leads to the item used.
    val owners = items.indices
      .flatMap(id => items(id).symbols.map(_ -> id))
      .toMap
    val mentions =
      items.indices.toVector.map(mentionsBy(_, items, owners))
    val executed = mentions.map(_.executed)
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
        if (items(user).isStrict) strictReads(user, items, executed) else Nil
      val candidates = (uses(user) ++ reads).distinct
      candidates.filter(used => items(used).isType || reads.contains(used))
    }
    val children = items.indices.toVector.map { user =>
      uses(user).filterNot(pulls(user).contains)
    }
    // An item waits for all its users to be placed, except those in its own
    // cycle, which would otherwise wait for each other for ever.
    val waits = Walk.reverse(children).map(_.toSet)
    val component = Walk.stronglyConnected(items.size, pulls, children)
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

  // Members grouped into items (see `Group`). Items come in the order their
  // first member was written, and a class goes before its companion.
  private def itemsOf(members: List[Member]): Vector[Item] =
    members.zipWithIndex
      .groupBy { case (member, index) => groupOf(member, index) }
      .values
      .toVector
      .sortBy(group => group.map { case (_, index) => index }.min)
      .map { group =>
        val written =
          group.sortBy { case (_, index) => index }.map { case (member, _) =>
            member.stat
          }
        val kept = source.keptRangeOf(written.head)
        val stats =
          if (kept.isDefined) written
          else
            written
              .sortBy(stat => if (stat.isInstanceOf[Defn.Object]) 1 else 0)
        val names = group.flatMap { case (member, _) => member.names }
        val symbols = group.flatMap { case (member, _) =>
          member.names.flatMap { name =>
            if (isClassLike(member.stat)) withCompanion(name.symbol)
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
            source = source.text.substring(lead.pos.start, lead.pos.end)
          ),
          isType = stats.exists(isType),
          isStrict = stats.exists(isStrict),
          symbols = symbols.toSet
        )
      }

  private def groupOf(member: Member, index: Int): Group = {
    val pairs =
      isClassLike(member.stat) || member.stat.isInstanceOf[Defn.Object]
    source.keptRangeOf(member.stat) match {
      case Some(range)   => Group.Kept(range)
      case None if pairs => Group.Companions(member.names.head.value)
      case None          => Group.Alone(index)
    }
  }

  // A class, trait or enum with no companion written out still has one in
  // SemanticDB, and it is what `Type(...)` and `Type.Case` resolve to. A
  // type alias has none, and an object of the same name is its own item.
  private def withCompanion(symbol: Symbol): List[Symbol] =
    List(symbol, Symbol(symbol.value.stripSuffix("#") + "."))

  // The other items an item uses, in the order its body first mentions
  // them once the scopes nested in it are in order (see `settledKey`); and,
  // of those, the ones whose code it runs: everything it mentions other than
  // as a bare type.
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
      stats.exists(covers(_, use.offset))
    }
    val kept = items(id).kept.isDefined
    val mentioned = (named ++ inferred)
      .sortBy { occurrence =>
        if (kept) List(Place.InHeader(occurrence.offset))
        else settledKey(stats, occurrence.offset)
      }
      .flatMap { occurrence =>
        val executes = !occurrence.symbol.value.endsWith("#")
        ownerIn(owners, occurrence.symbol).map(Mention(_, executes))
      }
      .filter(_.item != id)
    Mentions(
      used = mentioned.map(_.item).distinct,
      executed = mentioned.filter(_.executes).map(_.item).distinct
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
          Place.InItem(index) :: keyWithin(stat, offset)
      }
      .getOrElse(List(Place.InHeader(offset)))

  // Inside a definition, anything before its body (its header) comes first,
  // then its body's items in the order they will be in.
  private def keyWithin(stat: Stat, offset: Int): List[Place] =
    settledOrder(stat)
      .flatMap { case Ordered(items, order) =>
        order.zipWithIndex.collectFirst {
          case (id, position) if items(id).stats.exists(covers(_, offset)) =>
            val inside =
              if (items(id).kept.isDefined) List(Place.InHeader(offset))
              else settledKey(items(id).stats, offset)
            Place.InItem(position) :: inside
        }
      }
      .getOrElse(List(Place.InHeader(offset)))

  // The order the scope nested in a definition will end up in: its
  // canonical order, unless the rule can't rewrite it, which leaves it as
  // written (None).
  private def settledOrder(stat: Stat): Option[Ordered] =
    nested(stat).flatMap { scope =>
      val members = membersOf(scope)
      Some(apply(scope, members)).filter { ordered =>
        isInOrder(members, ordered) ||
        source.obstacle(ordered.items, ordered.order).isEmpty
      }
    }

  private def ownerIn(
      owners: Map[Symbol, ItemId],
      symbol: Symbol
  ): Option[ItemId] =
    Iterator
      .iterate(symbol)(_.owner)
      .takeWhile(s => !s.isNone && !s.isRootPackage && !s.isEmptyPackage)
      .collectFirst { case owner if owners.contains(owner) => owners(owner) }

  // The strict vals a strict val's initialiser reads, directly or through
  // what it runs, all of which have to be initialised before it. `executed`
  // leaves out mentions that only name a type, which run nothing, but
  // follows the rest into a type: constructing a class runs its body, and
  // touching a companion initialises it.
  private def strictReads(
      start: ItemId,
      items: Vector[Item],
      executed: Vector[List[ItemId]]
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
        else loop(executed(id) ++ rest, seen + id, found)
    }
    loop(executed(start), Set.empty, Nil)
  }
}

private[scalafix] object CanonicalOrder {

  // Which item a member belongs to.
  private sealed trait Group

  private object Group {
    // Any other definition, alone, by its position among the scope's members.
    final case class Alone(index: Int) extends Group

    // A class, trait or enum and its companion object, which share a name.
    final case class Companions(name: String) extends Group

    // Every definition in one kept range, which keeps its written order.
    final case class Kept(range: Span) extends Group
  }

  // `executes` is false where the item is only named as a type, which runs
  // none of its code.
  private final case class Mention(item: ItemId, executes: Boolean)

  private final case class Mentions(
      used: List[ItemId],
      executed: List[ItemId]
  )

  // One step of where an offset will end up, outermost first: at that offset
  // among what isn't an item (a definition's header, say, or anything inside
  // a kept range, which keeps its written order), or inside the item at a
  // position in its scope's settled order. A header comes before its body.
  private sealed trait Place

  private object Place {
    final case class InHeader(offset: Int) extends Place

    final case class InItem(position: Int) extends Place

    implicit val ordering: Ordering[Place] = Ordering.by {
      case InHeader(offset) => (0, offset)
      case InItem(position) => (1, position)
    }
  }
}
