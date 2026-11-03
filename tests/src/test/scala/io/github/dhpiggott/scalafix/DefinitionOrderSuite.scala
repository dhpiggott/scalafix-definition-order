package io.github.dhpiggott.scalafix

import org.scalatest.funsuite.AnyFunSuiteLike
import scalafix.testkit.AbstractSemanticRuleSuite

// One test per file under input: the rule runs over it and the result has to
// match the file of the same name under output, or, where there is none, leave
// the input unchanged and report exactly the diagnostics its `// assert:`
// comments name.
class DefinitionOrderSuite
    extends AbstractSemanticRuleSuite
    with AnyFunSuiteLike {
  runAllTests()
}
