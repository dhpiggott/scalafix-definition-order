# Contributing

Issues and pull requests are welcome.

A change to the rule should come with a test: add a file under
`input/src/main/scala/definitionorder/` headed `rule = DefinitionOrder`, and,
unless the rule should leave it unchanged, the expected result under the same
name in `output/`. Then run

```
sbt "scalafmtCheckAll; scalafmtSbtCheck; +rules/compile; tests/testFull; dogfood/scalafix --check DefinitionOrder"
```

which is what CI runs. Pull requests are squash-merged.
