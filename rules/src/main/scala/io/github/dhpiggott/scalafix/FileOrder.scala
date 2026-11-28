package io.github.dhpiggott.scalafix

import scala.meta.Position
import scala.meta.Stat

import io.github.dhpiggott.scalafix.Scopes.*
import io.github.dhpiggott.scalafix.SourceText.*
import scalafix.v1.*

// Rewrites one file: every scope in it that is out of order and can be
// rewritten is put in its canonical order, and every one that can't is
// reported.
private[scalafix] final class FileOrder(implicit doc: SemanticDocument) {

  private val source = new SourceText

  private val canonicalOrder = new CanonicalOrder(source)

  private var problems = List.empty[Patch]

  // The moves are one atomic patch: each edit only makes sense with all the
  // others, so if scalafix would drop any part of one, it drops them all.
  def patch: Patch = {
    val edits = scopes(doc.tree).flatMap(editsFor)
    val moves = edits.map(source.toPatch).asPatch.atomic
    (moves :: problems).asPatch
  }

  private def editsFor(scope: Scope): List[Edit] = {
    val members = membersOf(scope)
    val Ordered(items, order) = canonicalOrder(scope, members)
    // A kept range is one item however many definitions it holds, so it is
    // reported once, at its first definition, rather than once for each.
    items.filter(_.kept.isDefined).foreach { item =>
      problems = Patch.lint(KeptAsWritten(item.stats.head.pos)) :: problems
    }
    lazy val nestedEdits =
      items.filter(_.kept.isEmpty).flatMap(_.stats).flatMap(editsWithin)
    if (isInOrder(members, Ordered(items, order))) nestedEdits.toList
    else reordered(items, order).getOrElse(nestedEdits.toList)
  }

  private def editsWithin(stat: Stat): List[Edit] =
    nested(stat).toList.flatMap(editsFor)

  // The edits that rewrite the whole run of definitions in the canonical
  // order, with one blank line between them; or None, having reported why,
  // if the run can't be rewritten.
  private def reordered(
      items: Vector[Item],
      order: List[ItemId]
  ): Option[List[Edit]] = {
    val Run(pieces, run, indent, anchors) = source.runOf(items)
    source.obstacle(items, order) match {
      case Some(Obstacle(span, message)) =>
        report(Position.Range(doc.input, span.start, span.end), message)
        None
      case None =>
        // An anchor (a kept range, or an item holding a suppression) never
        // moves, so the rest of the run is rewritten around it. The anchors
        // split the run's text into segments (before the first, between each
        // pair, after the last), and split the canonical order into groups
        // the same way. Both splits are at the same anchored items, so each
        // segment has a group, and segment n is rewritten with group n's
        // definitions. Written a, K, b, c with the order b, K, c, a, the
        // segments hold [a] and [b, c], the groups are [b] and [c, a], and
        // the result is b, K, c, a.
        val separator = "\n\n" + indent
        val anchorEdges = anchors.flatMap { anchor =>
          List(anchor.block.start, anchor.block.end)
        }
        val bounds = run.start :: (anchorEdges :+ run.end)
        val segments = bounds.grouped(2).toList.collect { case List(from, to) =>
          Span(from, to)
        }
        val anchoredItems = anchors.flatMap(_.anchoredItem)
        // Where only one of a class and its companion holds a suppression,
        // the other still moves, to the anchored one's side: a class goes
        // at the end of the group above its companion, and a companion at
        // the start of the group below its class. The anchors are in the
        // canonical order, or `obstacle` would have reported them, so the
        // group above anchor n is group n.
        val (above, below) = anchoredItems.map { id =>
          val stats = items(id).stats
          val held = source.anchoredStats(items(id))
          val (before, after) = stats.span(stat => !held.contains(stat))
          (before, after.filterNot(held.contains))
        }.unzip
        val groups = splitAt(order, anchoredItems.toSet).zipWithIndex.map {
          case (ids, index) =>
            below.lift(index - 1).toList.flatten ++
              ids.flatMap(items(_).stats) ++
              above.lift(index).toList.flatten
        }
        // The anchors' own bodies. A kept range, such as `Log`'s levels, is
        // all suppressed, so there is nothing in it to edit. A definition
        // that holds a suppression, such as an object with a kept range
        // among the other definitions in its body, doesn't move, but those
        // other definitions are still ordered around the range inside it.
        val anchorEdits = anchoredItems
          .filter(items(_).kept.isEmpty)
          .flatMap(id => source.anchoredStats(items(id)))
          .flatMap(editsWithin)
        val last = segments.size - 1
        Some(anchorEdits ++ segments.zip(groups).zipWithIndex.flatMap {
          case ((segment, stats), index) =>
            // Where this segment's definitions start, as written. The
            // segments are cut at the anchors' edges, so every other
            // piece lies wholly inside one segment, and one that doesn't
            // start in this one is collected for the segment it's in.
            val written = pieces.collect {
              case Piece(block, None)
                  if segment.start <= block.start &&
                    block.start < segment.end =>
                block.start
            }
            // A segment already in order keeps its text, and only what
            // changes inside its definitions is edited. It's in order when
            // the definitions written in it, top to bottom, are its group's,
            // in the group's order: both lists are of block starts, which
            // tell definitions apart, and `pieces` is sorted by them.
            if (written == stats.map(source.blockOf(_).start))
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

  private def report(position: Position, message: String): Unit =
    problems = Patch.lint(Misplaced(position, message)) :: problems

  // The canonical order cut at each anchored item, which stays where it is.
  private def splitAt(
      order: List[ItemId],
      anchored: Set[ItemId]
  ): List[List[ItemId]] =
    order.foldRight(List(List.empty[ItemId])) { (id, groups) =>
      if (anchored(id)) Nil :: groups
      else (id :: groups.head) :: groups.tail
    }

  // A definition's block as it will read once the scopes nested in it are
  // rewritten too.
  private def render(stat: Stat): String = {
    val block = source.blockOf(stat)
    editsWithin(stat)
      .sortBy(-_.start)
      .foldLeft(source.text.substring(block.start, block.end)) {
        (rendered, edit) =>
          rendered.patch(
            edit.start - block.start,
            edit.text,
            edit.end - edit.start
          )
      }
  }
}
