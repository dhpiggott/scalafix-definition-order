#!/usr/bin/env bash
# Runs the rule, as built from this checkout, over another project's sources
# and prints what it changed and whether the result still compiles. Takes the
# project as owner/name. Used by trial.yml.
set -uo pipefail

repo=$1
version=0.0.0-trial
here=$(pwd)

sbt --batch --no-colors "set ThisBuild / version := \"$version\"" rules/publishLocal ||
  exit 1

git clone --quiet --depth 1 "https://github.com/$repo" "$RUNNER_TEMP/target"
cd "$RUNNER_TEMP/target" || exit 1

# sbt-scalafix needs sbt 1.x, and a plain build reads the same under it.
sed -i 's/^sbt\.version *= *2\..*/sbt.version=1.13.1/' project/build.properties
echo 'addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.9")' >> project/plugins.sbt
echo 'rules = [DefinitionOrder]' > .scalafix.conf

# `set every` reaches projects that turn SemanticDB off themselves.
fix=(
  "set every semanticdbEnabled := true"
  "set ThisBuild / scalafixDependencies += \"io.github.dhpiggott\" %% \"scalafix-definition-order\" % \"$version\""
)

echo "::group::scalafixAll"
sbt --batch --no-colors "${fix[@]}" scalafixAll 2>&1 | tee "$here/scalafix.log"
fixed=${PIPESTATUS[0]}
echo "::endgroup::"

git diff --stat -- . ':!project' ':!.scalafix.conf' > "$here/stat.txt"
git diff -- . ':!project' ':!.scalafix.conf' > "$here/diff.patch"

echo "::group::compile after the rewrite"
# The check doesn't need SemanticDB, but leaving it on keeps the compiler
# options the same as scalafixAll's, so sbt recompiles only what was rewritten
# instead of the whole build.
sbt --batch --no-colors "set every semanticdbEnabled := true" Test/compile 2>&1 | tail -40
compiled=${PIPESTATUS[0]}
echo "::endgroup::"

{
  echo "## $repo"
  echo
  echo "- scalafixAll exit: $fixed"
  echo "- compile after rewrite exit: $compiled"
  echo "- diagnostics: $(grep -c 'DefinitionOrder' "$here/scalafix.log")"
  echo
  echo '```'
  tail -n 30 "$here/stat.txt"
  echo '```'
} >> "$GITHUB_STEP_SUMMARY"

echo "::group::diagnostics"
grep -A3 'DefinitionOrder' "$here/scalafix.log" | head -200
echo "::endgroup::"
echo "::group::diff"
head -n 1500 "$here/diff.patch"
echo "::endgroup::"

# Fail the job if the rule crashed or its rewrite broke the build, so a red
# matrix entry is a finding to look at. Diagnostics alone don't fail it.
[ "$fixed" -eq 0 ] && [ "$compiled" -eq 0 ]
