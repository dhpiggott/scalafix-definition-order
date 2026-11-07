package io.github.dhpiggott.scalafix

import scala.annotation.tailrec
import scala.meta.Stat
import scala.meta.Token

import io.github.dhpiggott.scalafix.Scopes.*
import io.github.dhpiggott.scalafix.SourceText.*
import scalafix.v1.*

// A file as text: where its definitions and their comments start and end,
// which parts of it the rule mustn't edit, and how an edit to it becomes a
// patch. This is what decides whether a scope can be rewritten at all.
private[scalafix] final class SourceText(implicit doc: SemanticDocument) {

  // Declared before `keptRanges` and `okLines`, whose initialisers read it.
  private val tokens: Vector[Token] = doc.tokens.toVector

  val text: String = doc.input.text

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

  def keptRangeOf(stat: Stat): Option[Span] =
    keptRanges.find(_.contains(Span(stat.pos.start, stat.pos.end)))

  // Why a scope can't be rewritten into `order`, if it can't, and where.
  // Only whitespace may separate its definitions: anything else between two
  // of them has nowhere to go once they move.
  def obstacle(items: Vector[Item], order: List[ItemId]): Option[Obstacle] = {
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
  def runOf(items: Vector[Item]): Run = {
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

  // A definition together with the comments directly above it, and one
  // trailing it on its last line.
  def blockOf(stat: Stat): Span = {
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

  // Scalafix patches whole tokens, so a replacement becomes the first token
  // it covers replaced with the new text and the rest removed. That only
  // works if the edit starts exactly on a token; None means it doesn't. An
  // edit covering nothing is an insertion.
  def toPatch(edit: Edit): Option[Patch] =
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

private[scalafix] object SourceText {

  final case class Edit(start: Int, end: Int, text: String)

  // Why a scope can't be rewritten, and the part of the file to report it at.
  final case class Obstacle(span: Span, message: String)

  // Part of a run of definitions: one definition's block, or a whole kept
  // range's, along with the kept item it belongs to.
  final case class Piece(block: Span, keptItem: Option[ItemId])

  // A scope's definitions as written: the pieces, the span from the first to
  // the last, the indent the first starts at, and the kept pieces.
  final case class Run(
      pieces: List[Piece],
      span: Span,
      indent: String,
      anchors: List[Piece]
  )

  private final case class Marker(isOff: Boolean, comment: Token.Comment)
}
