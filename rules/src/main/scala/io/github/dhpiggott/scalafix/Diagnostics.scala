package io.github.dhpiggott.scalafix

import scala.meta.Position

import scalafix.lint.Diagnostic
import scalafix.lint.LintSeverity

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
