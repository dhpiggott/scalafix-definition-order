#!/usr/bin/env bash
# Runs the rule, as built from this checkout, over another project's sources
# and prints what it changed and whether the result still compiles. Takes the
# project as owner/name. Used by trial.yml.
set -uo pipefail

repo=$1
# A fixed version that Maven Central can never serve, so the target build can
# only resolve the rule just published locally. It also needs no tag history
# or extra sbt run to work out, as sbt-dynver's version would.
version=0.0.0-trial
here=$(pwd)

sbt --batch --no-colors "set ThisBuild / version := \"$version\"" rules/publishLocal ||
  exit 1

git clone --quiet --depth 1 "https://github.com/$repo" "$RUNNER_TEMP/target"
cd "$RUNNER_TEMP/target" || exit 1

# sbt-scalafix is published for sbt 1 and sbt 2 alike, so the project keeps
# whichever it uses.
echo 'addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.14.9")' >> project/plugins.sbt
echo 'rules = [DefinitionOrder]' > .scalafix.conf

# `set every` reaches projects that turn SemanticDB off themselves. Each sbt
# run is one `;`-separated argument because sbt 2's client joins separate
# arguments into a single command line, which sbt 1 doesn't.
semanticdb="set every semanticdbEnabled := true"
rule="set ThisBuild / scalafixDependencies += \"io.github.dhpiggott\" %% \"scalafix-definition-order\" % \"$version\""

# Whether the project compiles as cloned, under this JDK and with SemanticDB
# on. A project that doesn't can't show the rule breaking it. log4cats is one:
# its slf4j tests fail on a javac warning under -Werror, in a Java source the
# rule never touches.
echo "::group::compile before the rewrite"
sbt --batch --no-colors "$semanticdb; Test/compile" 2>&1 | tail -40
baseline=${PIPESTATUS[0]}
echo "::endgroup::"

echo "::group::scalafixAll"
sbt --batch --no-colors "$semanticdb; $rule; scalafixAll" 2>&1 | tee "$here/scalafix.log"
fixed=${PIPESTATUS[0]}
echo "::endgroup::"

git diff --stat -- . ':!project' ':!.scalafix.conf' > "$here/stat.txt"
git diff -- . ':!project' ':!.scalafix.conf' > "$here/diff.patch"

echo "::group::compile after the rewrite"
# The check doesn't need SemanticDB, but leaving it on keeps the compiler
# options the same as the runs before it, so sbt recompiles only what was
# rewritten instead of the whole build.
sbt --batch --no-colors "$semanticdb; Test/compile" 2>&1 | tail -40
compiled=${PIPESTATUS[0]}
echo "::endgroup::"

{
  echo "## $repo"
  echo
  echo "- compile before rewrite exit: $baseline"
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

# Fail the job if the rewrite broke a build that compiled before it, so a red
# matrix entry is a finding to look at. scalafixAll's own exit code isn't
# used: it is non-zero whenever the rule reports a scope it can't rewrite,
# which is expected, and the summary shows it alongside the diagnostics.
if [ "$baseline" -ne 0 ]; then
  echo "::warning::$repo doesn't compile before the rewrite, so the trial can't tell whether the rule broke it."
  exit 0
fi
[ "$compiled" -eq 0 ]
