inThisBuild(
  List(
    organization := "io.github.dhpiggott",
    homepage := Some(
      url("https://github.com/dhpiggott/scalafix-definition-order")
    ),
    licenses := List(License.Apache2),
    developers := List(
      Developer(
        "dhpiggott",
        "David Piggott",
        "david@piggott.me.uk",
        url("https://github.com/dhpiggott")
      )
    ),
    scalaVersion := scala213
  )
)

lazy val scala212 = _root_.scalafix.sbt.BuildInfo.scala212

lazy val scala213 = _root_.scalafix.sbt.BuildInfo.scala213

lazy val scala3 = "3.10.0"

// Lets the Scala 2.13 sources use what Scala 3 syntax 2.13 accepts, such as
// `import a.*` and `import a.{b as c}`. Scala 3 itself rejects the flag, so
// it goes on the 2.13 projects only. It also turns Scala 3 migration
// warnings into errors, and one of them fires on ordinary scalameta code:
// scalameta's extension methods (`tree.collect`, `input.syntax`) are found
// through a package prefix, which Scala 3 no longer searches. That only
// matters if the rule is ever compiled with Scala 3, and the fix would be
// importing scalameta internals by name, so that one warning is silenced.
lazy val scala3Syntax =
  Seq("-Xsource:3", "-Wconf:msg=found in a package prefix:s")

lazy val scalafixVersion = _root_.scalafix.sbt.BuildInfo.scalafixVersion

// Nothing to publish or check at the root, which only aggregates.
lazy val root = project
  .in(file("."))
  .aggregate(rules, input, output, tests)
  .settings(publish / skip := true)

// The rule. Scalafix loads a rule built for the Scala 2 version of the project
// it runs on (a Scala 3 project gets the 2.13 build), so it is published for
// 2.12 as well, for builds such as sbt 1 plugins.
lazy val rules = project
  .settings(
    moduleName := "scalafix-definition-order",
    crossScalaVersions := List(scala213, scala212),
    libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % scalafixVersion,
    scalacOptions ++= scala3Syntax
  )

// The test fixtures: each file under input is compiled with SemanticDB, run
// through the rule, and compared with the file of the same name under output,
// or, where there is none, expected to come out unchanged with exactly the
// diagnostics its `// assert:` comments name. They are written out of order
// and to trip warnings, so scalafmt (see .scalafmt.conf) and sbt-tpolecat's
// fatal warnings stay off them.
lazy val input = project
  .settings(
    scalaVersion := scala3,
    semanticdbEnabled := true,
    scalacOptions ~= (_.filterNot(Set("-Werror", "-Xfatal-warnings"))),
    publish / skip := true
  )

lazy val output = project
  .settings(
    scalaVersion := scala3,
    scalacOptions ~= (_.filterNot(Set("-Werror", "-Xfatal-warnings"))),
    publish / skip := true
  )

lazy val tests = project
  .enablePlugins(ScalafixTestkitPlugin)
  .dependsOn(rules)
  .settings(
    publish / skip := true,
    // The testkit is built against the compiler itself, so it's published once
    // per full Scala version (scalafix-testkit_2.13.18) rather than per binary
    // version (_2.13), and has to be asked for that way.
    libraryDependencies +=
      ("ch.epfl.scala" % "scalafix-testkit" % scalafixVersion % Test)
        .cross(CrossVersion.full),
    scalafixTestkitOutputSourceDirectories :=
      (output / Compile / unmanagedSourceDirectories).value,
    scalafixTestkitInputSourceDirectories :=
      (input / Compile / unmanagedSourceDirectories).value,
    scalafixTestkitInputClasspath :=
      (input / Compile / fullClasspath).value,
    scalafixTestkitInputScalacOptions :=
      (input / Compile / scalacOptions).value,
    scalafixTestkitInputScalaVersion :=
      (input / Compile / scalaVersion).value
  )

// The rule's own sources, compiled a second time with SemanticDB so the rule
// can be run over them. Scalafix can't load a rule into the project that
// compiles it, but it can load one from another project in the build. That's
// what `rules % ScalafixConfig` does: sbt-scalafix gives every project a
// ScalafixConfig configuration whose classpath is where scalafix looks for
// rules, and `% ScalafixConfig` puts the compiled `rules` project on that
// classpath instead of on this project's Compile classpath. So this project
// borrows `rules`' sources rather than having any of its own, and
// `dogfood/scalafix` checks them with the rule as it stands in the checkout,
// compiling `rules` first if it has changed.
lazy val dogfood = project
  .dependsOn(rules % ScalafixConfig)
  .settings(
    publish / skip := true,
    semanticdbEnabled := true,
    libraryDependencies += "ch.epfl.scala" %% "scalafix-core" % scalafixVersion,
    scalacOptions ++= scala3Syntax,
    Compile / unmanagedSourceDirectories :=
      (rules / Compile / unmanagedSourceDirectories).value
  )
