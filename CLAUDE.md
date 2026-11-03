# CLAUDE.md

`README.md` describes the rule for its users: the order, the opt-out, the
configuration and the limits. Read it first. This file only adds what matters
when working on the rule itself.

## Layout and checks

- `rules/` is the rule, compiled for Scala 2.13 because that is what scalafix
  loads rules on. `input/` and `output/` are the testkit fixtures (Scala 3), and
  `tests/` runs them. `sbt scalafmtCheckAll scalafmtSbtCheck tests/test` is what
  CI runs, and it runs on Linux, so a cloud session can run it too. Maven Central
  sometimes answers a cold dependency download there with HTTP 429; retrying
  after a pause gets through, because what did download stays cached.
- Every behaviour change comes with a fixture: an input headed
  `rule = DefinitionOrder`, plus an output of the same name unless the rule must
  leave the input unchanged, in which case its `// assert: DefinitionOrder`
  comments name the diagnostics it reports. The fixtures are excluded from
  scalafmt, because formatting one would change what it tests.
- The rule is not run on its own sources yet. Scalafix can't load a rule into
  the build that compiles it, so this has to wait for a release the build can
  take through `scalafixDependencies`.
- When the order itself changes, change the README's description of it in the
  same pull request. The header comment in `DefinitionOrder.scala` summarises
  it too.

## Releasing

`release.yml` runs `sbt ci-release`. A `v*` tag publishes that version to Maven
Central, and dispatching the workflow on `main` publishes a snapshot. The signing
key and Central token are secrets in the `release` environment, which only
`main` and `v*` tags can use. A cloud session can't push tags, so the maintainer
creates the tag and the GitHub release.

## Pull requests

`main` takes squash merges only, and `Build` must pass. Pull requests from
Dependabot and Scala Steward are merged by Mergify once `Build` passes.
