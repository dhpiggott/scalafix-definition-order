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
    val patches = edits.map(edit => (edit, source.toPatch(edit)))
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

  private def editsFor(scope: Scope): List[Edit] = {
    val members = membersOf(scope)
    val Ordered(items, order) = canonicalOrder(scope, members)
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

  // The whole run of definitions, rewritten in the canonical order with one
  // blank line between them, or None, having reported why, if it can't be.
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
        // The kept ranges stay put and cut the run into segments: before
        // the first, between each pair, and after the last. The canonical
        // order is cut at the same kept items, so segment n gets group n.
        val separator = "\n\n" + indent
        val bounds = run.start :: anchors.flatMap { anchor =>
          List(anchor.block.start, anchor.block.end)
        } ++ List(run.end)
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
