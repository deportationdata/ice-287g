# Summarize what an acquisition run changed under agreements/, sheets/ and manifests/
# from git's view of the staged tree -> $RUNNER_TEMP/287g-changes.txt, or the single
# line NO_REAL_CHANGES. The pass log alone is not a real change.
out_dir <- Sys.getenv("RUNNER_TEMP", tempdir())
out_path <- file.path(out_dir, "287g-changes.txt")

status <- system2(
  "git",
  c("status", "--porcelain=v1", "--untracked-files=all", "--", "agreements", "sheets", "manifests"),
  stdout = TRUE
)
status <- status[nzchar(status)]
code <- substr(status, 1, 2)
path <- substring(status, 4)
path <- sub("^.* -> ", "", path)   # a rename reports "old -> new"; the new path is what exists

kind <- ifelse(grepl("A|\\?", code), "added",
        ifelse(grepl("D", code), "removed",
        ifelse(grepl("R", code), "added", "modified")))
real <- path != "manifests/moa-passes.csv"

lines <- character()
section <- function(title, paths) {
  if (!length(paths)) return(character())
  c(paste0("**", length(paths), " file(s) ", title, ":**"), paste0("- ", paths), "")
}
lines <- c(
  section("added", path[real & kind == "added"]),
  section("removed", path[real & kind == "removed"]),
  section("with modified contents", path[real & kind == "modified"])
)

writeLines(if (length(lines)) lines else "NO_REAL_CHANGES", out_path)
cat(if (length(lines)) sprintf("%d changed path(s) summarized to %s\n", sum(real), out_path)
    else "NO_REAL_CHANGES\n")
