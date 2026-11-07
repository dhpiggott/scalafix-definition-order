# CLAUDE.md

`README.md` describes the rule for its users: the order, the opt-out, the
configuration and the limits. Read it first. This file only adds what matters
when working on the rule itself.

## Layout and checks

- `rules/` is the rule, compiled for Scala 2.13 because that is what scalafix
  loads rules on. `input/` and `output/` are the testkit fixtures (Scala 3), and
  `tests/` runs them. CI runs
  `sbt scalafmtCheckAll scalafmtSbtCheck tests/test "dogfood/scalafix --check DefinitionOrder"`
  on Linux, so a cloud session can run it too. Maven Central sometimes answers a
  cold dependency download there with HTTP 429; retrying after a pause gets
  through, because what did download stays cached. The workflows all take
  their JDK from `.java-version`. It only affects the build: scalac emits Java
  8 bytecode either way, so the published rule loads on any JDK from 8 up.
- Every behaviour change comes with a fixture: an input headed
  `rule = DefinitionOrder`, plus an output of the same name unless the rule must
  leave the input unchanged, in which case its `// assert: DefinitionOrder`
  comments name the diagnostics it reports. The fixtures are excluded from
  scalafmt, because formatting one would change what it tests.
- The rule is run on its own sources. Scalafix can't load a rule into the
  project that compiles it, so `dogfood` compiles `rules`' sources a second time
  and loads the rule from `rules` (`rules % ScalafixConfig`). After changing the
  rule's sources, `sbt "dogfood/scalafix DefinitionOrder" scalafmtAll` puts them
  back in order. One run settles them; if a second run moves anything, that's
  a bug in the rule (the first bug dogfooding found was exactly that).
- When the order itself changes, change the README's description of it in the
  same pull request. The header comment in `DefinitionOrder.scala` summarises
  it too.

## Trying it on other projects

`trial.yml` (dispatched by hand) builds the rule from the dispatched ref, runs
it over each project in its matrix with `.github/scripts/trial.sh`, prints the
rewrite and the diagnostics, and checks that the result still compiles. Run it
after a change to the order and read the diffs. Add a project to the matrix
rather than passing one in, so everything it runs is named in a commit.

## Releasing

`release.yml` runs `sbt ci-release`. A `v*` tag publishes that version to Maven
Central, and every push to `main` publishes a snapshot. The signing
key and Central token are secrets in the `release` environment, which only
`main` and `v*` tags can use. A cloud session can't push tags, so the maintainer
creates the tag and the GitHub release.

## Pull requests

`main` takes squash merges only, and `Build` must pass. Pull requests from
Dependabot and Scala Steward are merged by Mergify once `Build` passes.
