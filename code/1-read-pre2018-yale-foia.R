# Parse the archived Yale WIRAC 287(g) FOIA page -> data/pre2018-yale-foia-2008.csv.

suppressPackageStartupMessages({ library(rvest); library(dplyr); library(stringr) })

ROOT <- "."  # run from the repo root
HTML <- file.path(ROOT, "inputs", "pre2018", "yale_wirac_287g_foia_2008.html")
OUT  <- file.path(ROOT, "data"); dir.create(OUT, showWarnings = FALSE)
if (!file.exists(HTML)) stop("Missing ", HTML, " -- run code/0-287g-pre2018-download.R")

doc <- read_html(HTML)
a   <- html_elements(doc, "a")
df  <- tibble(label = str_squish(html_text2(a)), href = html_attr(a, "href")) |>
  filter(!is.na(href), str_detect(href, "287_g_foia")) |>
  filter(!str_detect(href, "\\.zip$")) |>
  filter(!str_detect(label, regex("^Letter from ICE", ignore_case = TRUE))) |>
  # the page carries a stray anchor whose text is punctuation only; drop anything without real letters
  filter(str_count(label, "[A-Za-z]") >= 3) |>
  distinct(href, .keep_all = TRUE)

# Expand the page's abbreviated jurisdiction labels (e.g. "Ala. Dept. of Pub. Safety")
expand <- function(x) {
  rep <- c("Ala\\."="Alabama","Az\\."="Arizona","Col\\."="Colorado","Cal\\."="California",
           "Fla\\."="Florida","Ga\\."="Georgia","Mass\\."="Massachusetts","Tenn\\."="Tennessee",
           "Tex\\."="Texas","Okla?\\."="Oklahoma","N\\.C\\."="North Carolina","N\\.H\\."="New Hampshire",
           "N\\.M\\."="New Mexico","S\\.C\\."="South Carolina","Ark\\."="Arkansas","Va\\."="Virginia",
           "Ok\\."="Oklahoma","Cty\\."="County","Dept\\."="Department","Pub\\."="Public",
           "Corr\\."="Corrections","Regional Adult Detention Center"="Regional Adult Detention Center")
  for (p in names(rep)) x <- str_replace_all(x, p, rep[[p]])
  str_squish(x)
}

yale <- df |>
  mutate(jurisdiction_label = expand(label),
         source_pdf = basename(href),
         pdf_recoverable = FALSE,       # verified: all Wayback captures are 404
         census_date = as.Date("2008-01-17"),
         source = "Yale WIRAC FOIA release (ICE production 2008-01-17)") |>
  select(jurisdiction_label, source_pdf, census_date, pdf_recoverable, source) |>
  arrange(jurisdiction_label)

write.csv(yale, file.path(OUT, "pre2018-yale-foia-2008.csv"), row.names = FALSE, na = "")
cat(sprintf("Yale WIRAC FOIA: %d agreements in force as of 2008-01-17\n", nrow(yale)))
if (nrow(yale) != 34) cat("WARNING: expected 34 MOUs; check the parse.\n")
print(yale$jurisdiction_label)
