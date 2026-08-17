# OCR the 1980 NLETS ORI Directory (OJP digitization, NCJ 75873) to recover
# ORIs for agencies absent from UCR/NIBRS-derived rosters (state DOCs,
# prosecutors, detention centers).

library(tidyverse)
library(pdftools)
library(tesseract)

pdf_url <- "https://www.ojp.gov/pdffiles1/Digitization/75873NCJRS.pdf"
pdf_path <- "inputs/nlets-ori-directory-1980.pdf"
checkpoint_path <- "data/nlets_ori_directory_ocr.rds"

if (!file.exists(pdf_path)) {
  options(timeout = 600)
  download.file(pdf_url, pdf_path, mode = "wb")
}

n_pages <- pdf_info(pdf_path)$pages

pages <- if (file.exists(checkpoint_path)) {
  readRDS(checkpoint_path)
} else {
  rep(NA_character_, n_pages)
}

engine <- tesseract("eng")

todo <- which(is.na(pages))
message(length(todo), " of ", n_pages, " pages left to OCR")

for (i in todo) {
  png <- pdf_convert(
    pdf_path,
    format = "png",
    pages = i,
    dpi = 300,
    filenames = tempfile(fileext = ".png"),
    verbose = FALSE
  )
  pages[i] <- ocr(png, engine = engine)
  unlink(png)

  if (i %% 25 == 0 || i == max(todo)) {
    saveRDS(pages, checkpoint_path)
    message("page ", i, " / ", n_pages)
  }
}

saveRDS(pages, checkpoint_path)

ocr_lines <- tibble(page = seq_along(pages), text = pages) |>
  separate_longer_delim(text, "\n") |>
  mutate(text = str_squish(text)) |>
  filter(text != "")

write_csv(ocr_lines, "data/nlets_ori_directory_lines.csv")

# the directory prints full 9-character ORIs; OCR of the microfiche scan
# garbles some characters (O/0, S/5, ID -> 1D), so keep the raw line for
# eyeballing and extract ORI-shaped tokens loosely
corrections_lines <- ocr_lines |>
  filter(
    str_detect(
      text,
      regex("CORR|PRISON|PENITEN|PENAL|DETENT", ignore_case = TRUE)
    )
  ) |>
  mutate(
    ori_tokens = map_chr(
      str_extract_all(str_to_upper(text), "\\b[A-Z0-9]{9}\\b"),
      \(x) paste(x, collapse = ";")
    )
  )

write_csv(corrections_lines, "data/nlets_ori_directory_corrections.csv")

message(
  nrow(corrections_lines),
  " corrections-related lines extracted; ",
  sum(corrections_lines$ori_tokens != ""),
  " with an ORI-shaped token"
)
