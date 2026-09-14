# ICE's own archive index of 287(g) MOAs: state, agency as ICE names it, original signing
# date and the archived file, from every dated snapshot of the page (0-acquire-archive-index.R
# adds one when the page changes); a document ICE later drops from the page stays evidence
# -> data/intermediate/historical-ice-archive-index.csv
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(readr); library(purrr) })

snapshots <- sort(list.files("inputs/historical", "^ice_live_287gMOA_index_\\d{4}-\\d{2}-\\d{2}\\.tsv$", full.names = TRUE))
idx <- snapshots |>
  map(\(src) read_tsv(src, col_names = c("state", "agency", "date_signed", "moa_file"),
                      col_types = cols(.default = "c"), progress = FALSE) |>
        mutate(retrieved = as.Date(str_extract(basename(src), "\\d{4}-\\d{2}-\\d{2}")))) |>
  list_rbind() |>
  mutate(across(c(state, agency, date_signed, moa_file), str_squish),
         date_signed = as.Date(date_signed),
         moa_file = na_if(moa_file, "")) |>
  arrange(retrieved) |>
  distinct(state, agency, date_signed, moa_file, .keep_all = TRUE) |>
  mutate(source = paste0("ICE 287(g) MOA archive index (retrieved ", as.integer(format(retrieved, "%d")), format(retrieved, " %b %Y"), ")")) |>
  select(source, state, agency, date_signed, moa_file)

stopifnot(
  "the archive index holds at least the 141 rows of its first snapshot" = nrow(idx) >= 141L,
  "every archive row names a state and an agency" = !anyNA(idx$state) && !anyNA(idx$agency)
)
write_csv(idx, "data/intermediate/historical-ice-archive-index.csv", na = "")
cat(sprintf("ICE archive index: %d agreements, %d dated, %d with a file\n",
            nrow(idx), sum(!is.na(idx$date_signed)), sum(!is.na(idx$moa_file))))
