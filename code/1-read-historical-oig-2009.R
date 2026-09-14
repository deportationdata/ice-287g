# DHS OIG-10-63 Appendix E, Table 3: every 287(g) jurisdiction as of 28 Oct 2009
# with model, original signing date and signed/pending status
# -> data/historical-oig-2009.csv
suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(purrr)
  library(readr)
})

pdf <- "inputs/historical/reports/DHS-OIG-10-63_Mar2010.pdf"
if (!nzchar(Sys.which("pdftotext"))) {
  message(
    "pdftotext not available; keeping committed data/historical-oig-2009.csv"
  )
  quit(save = "no", status = 0)
}
tf <- tempfile(fileext = ".txt")
system2(
  "pdftotext",
  c("-layout", shQuote(pdf), shQuote(tf)),
  stdout = FALSE,
  stderr = FALSE
)
L <- readLines(tf, warn = FALSE, encoding = "UTF-8")

# the table spans three pages, each restating the header
starts <- grep("^Table 3\\. Jurisdictions Participating|^Appendix E$", L)
first <- grep("^Table 3\\. Jurisdictions Participating", L)[1]
last <- grep("^Source: ICE OSLC", L)
last <- last[last > first][1]
blk <- L[first:last]

states <- c(state.name, "District of Columbia")
# a mark in the Jail or Task Force column is a glyph pdftotext cannot name; its
# position against the header decides which column it sits in
header <- blk[str_detect(blk, "\\bJail\\b")][1]
force_header <- blk[str_detect(blk, "\\bForce\\b")][1]
jail_at <- str_locate(header, "Jail")[1, 1]
force_at <- str_locate(force_header, "Force")[1, 1]
mid <- (jail_at + force_at) / 2

row_pat <- "^\\s{2,}(\\S.*?\\S)\\s{2,}(.*?)\\s*(\\d{1,2}/\\d{1,2}/\\d{4})?\\s+(Signed|Pending)\\s*$"
cur <- NA_character_
rows <- list()
for (ln in blk) {
  st <- str_squish(ln)
  if (st %in% states) {
    cur <- st
    next
  }
  m <- str_match(ln, row_pat)
  if (is.na(m[1])) {
    next
  }
  marks <- str_locate_all(ln, "[^\\x20-\\x7E]")[[1]][, 1]
  rows[[length(rows) + 1]] <- tibble(
    state = cur,
    agency = str_squish(m[2]),
    jail = any(marks < mid),
    task_force = any(marks >= mid),
    date_signed_original = as.Date(m[4], "%m/%d/%Y"),
    status = str_to_lower(m[5])
  )
}
oig <- bind_rows(rows) |>
  mutate(
    model = case_when(
      jail & task_force ~ "Hybrid",
      jail ~ "Jail Enforcement",
      task_force ~ "Task Force",
      TRUE ~ NA_character_
    ),
    source = "DHS OIG-10-63 App. E Table 3 (28 Oct 2009)",
    as_of = as.Date("2009-10-28")
  ) |>
  select(source, as_of, state, agency, model, date_signed_original, status)

stopifnot(
  "OIG App. E lists 67 jurisdictions" = nrow(oig) == 67L,
  "OIG App. E has six pending agreements" = sum(oig$status == "pending") == 6L,
  "every OIG row carries a state" = !anyNA(oig$state),
  "every signed row carries its original signing date" = !anyNA(oig$date_signed_original[
    oig$status == "signed"
  ])
)
write_csv(oig, "data/intermediate/historical-oig-2009.csv", na = "")
cat(sprintf(
  "OIG-10-63 App. E: %d jurisdictions (%d signed, %d pending), %d states\n",
  nrow(oig),
  sum(oig$status == "signed"),
  sum(oig$status == "pending"),
  n_distinct(oig$state)
))
