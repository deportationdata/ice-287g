#!/bin/bash
# Summarize what an acquisition run staged for its PR, under the roots given as arguments (default agreements
# and sheets); run after `git add`. Prints nothing when the only change is the MOA pass log (agreements/moa-passes.csv).
[ $# -gt 0 ] || set -- agreements sheets
git status --porcelain=v1 --untracked-files=all -- "$@" | awk '
  function section(kind, title) {
    if (n[kind]) printf "**%d file(s) %s:**\n%s\n", n[kind], title, list[kind]
  }
  NF == 0 { next }
  {
    code = substr($0, 1, 2)
    path = substr($0, 4)
    sub(/^.* -> /, "", path)   # a rename reports "old -> new"
    if (path == "agreements/moa-passes.csv") next
    if (code ~ /A|\?/) kind = "added"
    else if (code ~ /D/) kind = "removed"
    else if (code ~ /R/) kind = "added"
    else kind = "modified"
    n[kind]++
    list[kind] = list[kind] "- " path "\n"
  }
  END {
    section("added", "added")
    section("removed", "removed")
    section("modified", "with modified contents")
  }'
