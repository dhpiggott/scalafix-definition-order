# scalafix-definition-order

A [Scalafix](https://scalacenter.github.io/scalafix/) rule that puts the
definitions in each scope of a Scala file into one canonical order, and moves
them there for you. It follows the "newspaper" layout from chapter 5 of *Clean
Code*: a file reads from top to bottom, starting at its entry points and ending
at its smallest helpers.

The order is worked out from the code alone, so it never matters what order the
definitions were written in. `scalafix DefinitionOrder` rewrites a file into
that order, and `scalafix --check DefinitionOrder` fails if a file isn't in it.

## Example

```scala
def helper: Int = 1

def entry: Int = helper + other

def other: Int = 2
```

becomes

```scala
def entry: Int = helper + other

def helper: Int = 1

def other: Int = 2
```

## Installation

The rule is a semantic rule, so SemanticDB has to be enabled. With
[sbt-scalafix](https://scalacenter.github.io/scalafix/docs/users/installation.html):

```scala
// build.sbt
ThisBuild / semanticdbEnabled := true
ThisBuild / scalafixDependencies +=
  "io.github.dhpiggott" %% "scalafix-definition-order" % "<version>"
```

```hocon
# .scalafix.conf
rules = [DefinitionOrder]
```

Then `sbt scalafixAll` rewrites your sources, and `sbt "scalafixAll --check"`
fails the build on anything out of place. The tests are Scala 3 sources,
including the fewer-braces syntax. Scala 2.13 sources should work, but nothing
tests them yet.

## The order

A scope is a file's top level, or the body of one object, class, trait, enum or
extension block. Each scope has exactly one order:

- **A term goes below everything in its scope that uses it**, as soon as all of
  them have been placed. The rule walks depth first from each root, visiting
  what a definition uses in the order its body first mentions it. Terms are
  `def`s, `val`s, `var`s, `object`s and extension blocks. A helper with several
  callers sits below the last of them.
- **A type goes immediately above the first definition that names it.** Types
  are classes, traits, enums and type aliases. A class and its companion object
  are one definition, with the class first.
- **Initialisation order beats both.** Top-level and object `val`s initialise
  in file order, so a strict `val` that an initialiser reads goes immediately
  above that initialiser. That holds whether the initialiser reads it directly
  or through what it runs, even where this puts the `val` above a `def` that
  uses it. What an initialiser runs includes the `def`s it calls, the body of a
  class it constructs, and the companion object it touches. A type it only names
  runs nothing.
- **Whatever the uses leave open goes public before private, then by name.**
  That covers a scope's roots (entry points, and siblings that share no use) and
  the members of a cycle that no root leads into.
- **A class, trait or enum body puts its fields (strict `val`s and `var`s)
  first**, then everything else. Each part is ordered as above.

## How this relates to *Clean Code*

The order follows the vertical formatting rules in chapter 5 of *Clean Code*,
with some decisions of its own where the chapter is silent or Scala doesn't
allow it.

What it takes from the chapter:

- **The newspaper metaphor and vertical ordering.** High-level code comes
  first and detail further down, so a function that is called sits below the
  one that calls it. The depth-first walk puts definitions in the order the
  chapter's own examples use.
- **Dependent functions.** The chapter says "the caller should be above the
  callee, if at all possible". This rule makes that mandatory.
- **Instance variables at the top of the class.** This is the "fields first"
  rule.
- **Team rules.** There is exactly one order, so there is nothing to agree on
  or argue about in review.

Where it goes its own way:

- **Shared helpers.** The chapter wants a callee close to its caller, but says
  nothing about a function with several callers. This rule puts it below the
  last of them, which can leave it some way from the first.
- **Conceptual affinity.** The chapter groups related functions that don't
  call each other, such as a family of overloads. Nothing in the code tells the
  rule they are related, so it orders them public before private, then by
  name. Wrap them in `scalafix:off` to keep a grouping like that.
- **Initialisation order.** A strict `val` must come before anything that
  reads it while initialising, even when that puts it above a `def` that uses
  it. This is how Scala works, not a style choice, and the chapter doesn't
  cover it.
- **Types.** A type goes immediately above the first definition that names it.
  The chapter doesn't say where types go. This is the closest match to its
  advice to declare a variable close to where it is used.

Vertical openness, vertical density and horizontal formatting, the rest of the
chapter, are a formatter's job.

## Comments

A comment moves with the definition directly below it, as long as no blank line
separates them. So does a trailing comment on the definition's last line. A
comment that covers several definitions moves with the first of them only, so
word it to stand on its own.

In two cases the rule can't rewrite a scope safely, so it reports the scope as
out of order and leaves it for you to fix by hand:

- **Something that isn't a definition sits between two definitions**, such as
  a comment with a blank line below it, an enum `case`, or an anonymous
  `given`. The rule doesn't know which definition it belongs with, so it has
  nowhere to move it to.
- **A definition that has to move contains a `scalafix:off` range or a
  `scalafix:ok` comment.** Scalafix discards any edit inside one of those, so
  only part of the move would be applied.

## Keeping an order the code can't express

Wrap definitions in `// scalafix:off DefinitionOrder` …
`// scalafix:on DefinitionOrder` to keep them in their written order, for
example log levels listed from most to least severe:

```scala
object Log:
  // Most to least severe, which no use between them says.
  // scalafix:off DefinitionOrder
  def error(message: String): Unit = ...
  def warn(message: String): Unit = ...
  def info(message: String): Unit = ...
  // scalafix:on DefinitionOrder
```

The definitions between the two comments are ordered as one item, and the rest
of the scope is arranged around them. Scalafix drops any patch inside such a
range, so the rule only ever edits either side of it. Add a comment saying why,
since the rule can't.

## Configuration

```hocon
# Regexes matched against each file's path. A matching file is left alone.
DefinitionOrder.exclude = ["src/main/scala/vendored/", "/Generated\\.scala$"]
```

## Limitations

- The rule only sees uses that SemanticDB records. That includes what the
  compiler fills in, read from SemanticDB's synthetics, so a `given` passed as
  a context argument counts as used where it is passed.
- A use from another scope doesn't count. Initialisation order across scopes is
  still yours to watch.

## Development

The tests use
[scalafix-testkit](https://scalacenter.github.io/scalafix/docs/developers/setup.html).
Every file under `input/` (headed `rule = DefinitionOrder`) is compiled with
SemanticDB, run through the rule, and compared with the file of the same name
under `output/`. An input with no output file must come out unchanged, with
exactly the diagnostics its `// assert: DefinitionOrder` comments name.

```
sbt tests/test
```

## Licence

[Apache 2.0](LICENSE).
