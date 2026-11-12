package io.github.dhpiggott.scalafix

import scala.annotation.tailrec

import io.github.dhpiggott.scalafix.Scopes.*

// The depth-first walk. `pulls` are what an item needs directly above it
// (types it names, strict vals its initialiser reads), placed just before
// it; `children` are what it uses, each placed once everything that uses it
// outside its own cycle has been. `waits` is `children` reversed: for each
// item, the items it is a child of, which is what it waits for before it
// can be placed (see `isReady`). `component(id)` numbers the cycle an item
// is in (its strongly connected component), so two items with the same
// number are in one cycle, and an item in no cycle has a number of its own.
private[scalafix] final class Walk(
    items: Vector[Item],
    pulls: Vector[List[ItemId]],
    children: Vector[List[ItemId]],
    waits: Vector[Set[ItemId]],
    component: Vector[Int]
) {

  private val placed = Array.fill(items.size)(false)

  private val pulled = pulls.flatten.toSet

  private val pullers = Walk.reverse(pulls)

  private var reversed = List.empty[ItemId]

  // Anything the walk didn't reach from a root, such as the members of a
  // cycle no root leads into: the first in key order that is free. One
  // always is while any are left, since the cycles the edges form can be
  // ordered top to bottom, and the members of the top one are free. If none
  // is, the edges are wrong, which is a bug in the rule.
  @tailrec def finish(phase: Set[ItemId]): Unit = {
    val remaining = phase.filterNot(placed(_)).toList.sortBy(items(_).key)
    if (remaining.nonEmpty) {
      val next = remaining.find(isFree).getOrElse {
        throw new IllegalStateException(
          s"No definition is free to place next among ${remaining.size}. " +
            "This is a bug in the rule; please report it."
        )
      }
      visit(next, phase)
      finish(phase)
    }
  }

  // Whether the item can go next. It must be ready, so it won't land above
  // one of its users. And nothing still to be placed may pull it: what
  // pulls an item places it directly above itself, so placing it now would
  // leave it stranded further up. A puller in the item's own cycle doesn't
  // count, as with two types that name each other, which pull each other.
  private def isFree(id: ItemId): Boolean =
    isReady(id) &&
      pullers(id).forall(puller =>
        placed(puller) || component(puller) == component(id)
      )

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

  // Whether every user of the item has been placed, so it can go next
  // without landing above one. Users in its own cycle don't count, since
  // the cycle's members can't all go below each other.
  def isReady(id: ItemId): Boolean =
    waits(id).forall(user => placed(user) || component(user) == component(id))

  def isPulled(id: ItemId): Boolean = pulled(id)

  def order: List[ItemId] = reversed.reverse
}

private[scalafix] object Walk {

  // Tarjan's algorithm over both kinds of edge, each pointing from the item
  // that goes above to the one that goes below.
  def stronglyConnected(
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
  def reverse(edges: Vector[List[ItemId]]): Vector[List[ItemId]] = {
    val from = Array.fill(edges.size)(List.empty[ItemId])
    edges.indices.reverse.foreach { id =>
      edges(id).distinct.foreach(to => from(to) = id :: from(to))
    }
    from.toVector
  }
}
