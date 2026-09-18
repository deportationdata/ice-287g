# Snapshot manifests and MOA url repair; needs only stringr, purrr, dplyr, readr.

# MOA links are hand-pasted: unwrap Outlook safelinks and repair paste typos
clean_moa_urls <- function(url) {
  url |>
    map_chr(\(u) {
      if (is.na(u)) {
        NA_character_
      } else if (str_detect(u, "safelinks\\.protection\\.outlook\\.com")) {
        inner <- str_extract(u, "(?<=[?&]url=)[^&]+")
        if (is.na(inner)) u else URLdecode(inner)
      } else {
        u
      }
    }) |>
    str_remove("^chrome-extension://[a-z]+/") |>
    str_replace("^https?:/(?=[^/])", "https://") |>
    str_replace("^http://", "https://")
}

# one manifest.csv per snapshot folder; 0-acquire-manifests.R writes one for any folder
# without it, from a download_path_log.csv where the folder has one, else from its files.
# The validator columns let a later pass ask the origin "changed?" without a body.
manifest_columns <- c(
  "saved_path", "file_hash", "url", "retrieved_at",
  "state", "agency", "original_filename", "note",
  "etag", "last_modified", "capture_time"
)

# one comparable key per document: the decoded file name, with case, punctuation and any
# 12-hex hash suffix before .pdf all ignored
doc_key <- function(x) {
  x |> str_remove("\\?.*$") |> basename() |> map_chr(\(n) tryCatch(URLdecode(n), error = \(e) n)) |>
    str_to_lower() |> str_remove("_[0-9a-f]{12}(?=\\.pdf$)") |> str_replace_all("[^a-z0-9]", "")
}

# ICE names each MOA PDF after the agency, state, model and signing date
# (LoudonCoSOTN_TFM_MOA_06222026.pdf); these forms reproduce 84% of 2026 filenames, most
# productive first (a backtest on every MOA posted for agreements signed since June 2025):
# County -> Co, Department -> Dept, then the SO/PD/SD abbreviations and two-digit or dotted
# dates. The probe asks ice.gov for them; the build looks a pending agreement's up among the
# PDFs already held, and a name built from one agreement can be no other's
moa_candidate_files <- function(agency, st, model, signed) {
  words <- agency |> str_replace_all("[’']", "") |> str_replace_all("&", "and") |>
    str_replace_all("[^A-Za-z0-9 ]", " ") |> str_squish() |> str_split(" ") |> unlist()
  camel <- paste0(toupper(substr(words, 1, 1)), substring(words, 2), collapse = "")
  co <- str_replace_all(camel, "County", "Co")
  dept <- str_replace_all(co, "Department", "Dept")
  forms <- list(c(dept, "%m%d%Y"), c(str_replace(dept, "PoliceDept$", "PD"), "%m%d%Y"),
                c(str_replace(co, "SheriffsOffice$", "SO"), "%m%d%Y"), c(str_replace(dept, "PoliceDept$", "PD"), "%m%d%y"),
                c(str_replace(co, "SheriffsOffice$", "SO"), "%m%d%y"), c(str_replace(dept, "SheriffsDept$", "SD"), "%m%d%Y"),
                c(dept, "%m.%d.%Y"), c(co, "%m%d%Y"))
  unique(map_chr(forms, \(f) paste0(f[1], st, "_", model, "_MOA_", format(signed, f[2]), ".pdf")))
}

read_manifest <- function(path) {
  read_csv(path, col_types = readr::cols(.default = "c"), progress = FALSE)
}

append_manifest <- function(folder, rows) {
  path <- file.path(folder, "manifest.csv")
  if (file.exists(path)) {
    rows <- bind_rows(read_manifest(path), rows) |>
      distinct(saved_path, .keep_all = TRUE)
  }
  for (col in manifest_columns) {
    if (!col %in% names(rows)) rows[[col]] <- NA_character_
  }
  write_csv(rows[manifest_columns], path, na = "")
}

# every manifest under a snapshot root, with its folder and the path the row resolves to
# today (saved_path is root-relative for scraped rows, folder-relative for reconstructed ones)
snapshot_manifests <- function(root = "agreements") {
  m <- list.files(root, "^manifest\\.csv$", recursive = TRUE, full.names = TRUE) |>
    map(\(p) read_manifest(p) |> mutate(folder = dirname(p), .before = 1)) |>
    list_rbind()
  # a manifest missing any column gets it as NA
  for (col in manifest_columns) if (!col %in% names(m)) m[[col]] <- NA_character_
  m |>
    mutate(
      path_now = if_else(file.exists(saved_path), saved_path, file.path(folder, basename(saved_path))),
      on_disk = file.exists(path_now)
    )
}

# The newest participating-agencies workbook, by the manifest's note, else by its header
# row (only participating lists have SIGNED); never by filename, which ICE changes
newest_sheet_snapshot <- function(root = "sheets") {
  has_roster_header <- function(path) {
    hdr <- tryCatch(names(readxl::read_excel(path, n_max = 0)), error = function(e) character())
    all(c("STATE", "LAW ENFORCEMENT AGENCY", "SIGNED") %in% toupper(str_squish(hdr)))
  }
  for (folder in rev(sort(list.files(root, "^sheets_2", full.names = TRUE)))) {
    workbooks <- list.files(folder, "\\.xlsx$", full.names = TRUE, ignore.case = TRUE)
    if (length(workbooks) == 0) next
    noted <- character()
    if (file.exists(file.path(folder, "manifest.csv"))) {
      rows <- read_manifest(file.path(folder, "manifest.csv"))
      noted <- file.path(folder, basename(rows$saved_path[str_detect(coalesce(rows$note, ""), "^participating")]))
    }
    sheet <- c(intersect(noted, workbooks), keep(workbooks, has_roster_header))[1]
    if (!is.na(sheet)) return(sheet)
  }
  stop("no participating-agencies workbook in any sheets snapshot")
}

# url -> the bytes we hold for it, their validators, and where one copy still lives;
# derived from the manifests every run, so it can never disagree with the files
moa_validator_map <- function(m = snapshot_manifests("agreements")) {
  kept <- m |>
    filter(on_disk) |>
    distinct(file_hash, .keep_all = TRUE) |>
    select(file_hash, on_disk_path = path_now)
  m |>
    filter(!is.na(url), url != "") |>
    # the sheet links plain https urls; the archive may hold the same document
    # under a Wayback-prefixed or http:// url, and those bytes count as held
    mutate(url = str_remove(url, "^https?://web\\.archive\\.org/web/\\d+id_/") |>
                    str_replace("^http://", "https://")) |>
    arrange(url, retrieved_at) |>
    summarise(
      file_hash = dplyr::last(file_hash),
      retrieved_at = dplyr::last(retrieved_at),
      etag = dplyr::last(na.omit(etag)),
      last_modified = dplyr::last(na.omit(last_modified)),
      .by = url
    ) |>
    left_join(kept, by = "file_hash", relationship = "many-to-one")
}

# a manifest row whose file dedupe removed keeps its row but must say where the
# identical bytes live; without this a 1,888-row manifest over an empty folder is a lie
annotate_ghost_rows <- function(root = "agreements", folders = NULL) {
  m <- snapshot_manifests(root)
  if (!nrow(m)) return(invisible(0L))
  kept <- m |>
    filter(on_disk) |>
    distinct(file_hash, .keep_all = TRUE) |>
    select(file_hash, kept_path = path_now)
  todo <- m |>
    filter(!on_disk, is.na(note) | !str_detect(note, "deduplicated|deleted;")) |>
    left_join(kept, by = "file_hash", relationship = "many-to-one")
  if (!is.null(folders)) todo <- filter(todo, folder %in% folders)
  n <- 0L
  for (f in unique(todo$folder)) {
    rows <- read_manifest(file.path(f, "manifest.csv"))
    t <- filter(todo, folder == f)
    i <- match(t$saved_path, rows$saved_path)
    tag <- if_else(
      is.na(t$kept_path),
      "deleted; no identical bytes found",
      paste0("deduplicated ", Sys.Date(), "; identical bytes retained at ", t$kept_path)
    )
    rows$note[i] <- if_else(is.na(rows$note[i]) | rows$note[i] == "", tag,
                                   paste(rows$note[i], tag, sep = "; "))
    for (col in manifest_columns) if (!col %in% names(rows)) rows[[col]] <- NA_character_
    write_csv(rows[manifest_columns], file.path(f, "manifest.csv"), na = "")
    n <- n + length(i)
  }
  invisible(n)
}
