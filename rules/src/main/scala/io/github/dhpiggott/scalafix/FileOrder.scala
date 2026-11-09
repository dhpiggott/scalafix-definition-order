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
        // A kept range never moves, so the rest of the run is rewritten
        // around it. The kept ranges split the run's text into segments
        // (before the first, between each pair, after the last), and split
        // the canonical order into groups the same way. Both splits are at
        // the same kept items, so each segment has a group, and segment n is
        // rewritten with group n's definitions. Written a, K, b, c with the
        // order b, K, c, a, the segments hold [a] and [b, c], the groups are
        // [b] and [c, a], and the result is b, K, c, a.
        val separator = "\n\n" + indent
        val keptEdges = anchors.flatMap { anchor =>
          List(anchor.block.start, anchor.block.end)
        }
        val bounds = (run.start :: keptEdges) :+ run.end
        val segments = bounds.grouped(2).toList.collect { case List(from, to) =>
          Span(from, to)
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
