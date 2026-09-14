# ICE's own agency lists from before the dated roster table: the 287(g) partners
# page carried an undated list "as of 9-19-07" (28 agencies) and one "updated
# 3-10-08" (41 agencies) before the STATE / AGENCY / SUPPORT / SIGNED table
# appeared in April 2008 -> data/historical-ice-lists.csv
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(purrr); library(readr); library(xml2) })

DIR <- "sheets/sheets_wayback_pre2011"
xwalk <- arrow::read_parquet("data/intermediate/reference-state-codes.parquet")

files <- list.files(DIR, "^\\d{14}_s\\d\\.html$", full.names = TRUE)
if (!length(files)) stop("no pre-2011 captures under ", DIR, "; run code/0-acquire-wayback.R first")

head_pat <- "(?i)(signed MOAs as of|agencies with signed MOAs \\(updated)\\s*(\\d{1,2}-\\d{1,2}-\\d{2,4})"

read_list <- function(path) {
  lines <- read_html(path) |>
    xml_find_all("//body//text()") |>
    xml_text() |>
    str_squish()
  lines <- lines[nzchar(lines)]
  hdr <- which(str_detect(lines, head_pat))
  if (!length(hdr)) return(NULL)
  as_of <- as.Date(str_match(lines[hdr[1]], head_pat)[, 3], tryFormats = c("%m-%d-%y", "%m-%d-%Y"))
  body <- lines[(hdr[1] + 1):length(lines)]
  is_row <- str_detect(body, "^[A-Z]{2} \\S")
  # the list ends at the first non-matching line after it starts
  stop_at <- which(!is_row & cumsum(is_row) > 0)[1]
  rows <- body[seq_len(if (is.na(stop_at)) length(body) else stop_at - 1)]
  # a missing line break runs two agencies into one node ("... Sheriff's Office FL Collier
  # County ..."); a state code after a lowercase word starts a new row. ICE's page
  # encodes its apostrophes as a replacement character
  rows <- unlist(str_split(rows, "(?<=[a-z'.]\\s)(?=[A-Z]{2} [A-Z])")) |>
    str_replace_all("�", "'") |>
    # "Sheriff 's" and "Sherrif's" are the page's own typos
    str_replace_all(" 's", "'s") |>
    str_replace_all("Sherrif", "Sheriff")
  rows <- rows[str_detect(rows, "^[A-Z]{2} \\S")]
  tibble(capture = str_match(basename(path), "^(\\d{14})")[, 2], as_of = as_of,
         state_abbr = str_sub(rows, 1, 2), agency = str_squish(str_sub(rows, 4)))
}

lists <- map(files, read_list) |> list_rbind() |>
  # the same list can sit in several captures; keep it once, under its earliest capture
  group_by(as_of) |> filter(capture == min(capture)) |> ungroup() |>
  left_join(xwalk |> select(state_abbr, state = state_full), by = "state_abbr") |>
  mutate(source = sprintf("ICE 287(g) partners page, agency list as of %s (Wayback %s)", as_of, capture)) |>
  select(source, capture, as_of, state, state_abbr, agency) |>
  arrange(as_of, state, agency)

counts <- lists |> count(as_of)
stopifnot(
  "two undated ICE lists: 2007-09-19 and 2008-03-10" =
    setequal(as.character(counts$as_of), c("2007-09-19", "2008-03-10")),
  "the 2007-09-19 list names 28 agencies" = counts$n[counts$as_of == as.Date("2007-09-19")] == 28L,
  "the 2008-03-10 list names 41 agencies" = counts$n[counts$as_of == as.Date("2008-03-10")] == 41L,
  "every row resolves its state" = !anyNA(lists$state)
)
write_csv(lists, "data/intermediate/historical-ice-lists.csv", na = "")
cat(sprintf("ICE undated lists: %s\n", paste(sprintf("%s (%d)", counts$as_of, counts$n), collapse = ", ")))
