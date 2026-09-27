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

# one manifest.csv per snapshot folder; validators let a later pass ask "changed?" without a body
manifest_columns <- c(
  "saved_path", "file_hash", "url", "retrieved_at",
  "state", "agency", "original_filename", "note",
  "etag", "last_modified", "capture_time"
)

# a document's key: decoded file name, ignoring case, punctuation and any 12-hex hash suffix
doc_key <- function(x) {
  x |> str_remove("\\?.*$") |> basename() |> map_chr(\(n) tryCatch(URLdecode(n), error = \(e) n)) |>
    str_to_lower() |> str_remove("_[0-9a-f]{12}(?=\\.pdf$)") |> str_replace_all("[^a-z0-9]", "")
}

# candidate MOA PDF names from agency, state, model and signing date, most productive form first;
# County -> Co, Department -> Dept, SO/PD/SD, two-digit or dotted dates cover 84% of 2026 filenames
moa_candidate_files <- function(agency, st, model, signed) {
  words <- agency |> str_replace_all("[’']", "") |> str_replace_all("&", "and") |>
    str_replace_all("[^A-Za-z0-9 ]", " ") |> str_squish() |> str_split(" ") |> unlist()
  camel <- paste0(toupper(substr(words, 1, 1)), substring(words, 2), collapse = "")
  # ICE's filename can name a sheriff's office a department or the reverse
  # (LoganCoSheriffsOfficeKS for the sheet's Logan County Sheriff's Department),
  # so the other spelling's names follow the sheet's own
  swapped <- if (str_detect(camel, "SheriffsOffice$")) str_replace(camel, "Office$", "Department")
             else if (str_detect(camel, "SheriffsDepartment$")) str_replace(camel, "Department$", "Office")
  files <- map(c(camel, swapped), \(camel) {
    co <- str_replace_all(camel, "County", "Co")
    dept <- str_replace_all(co, "Department", "Dept")
    forms <- list(c(dept, "%m%d%Y"), c(str_replace(dept, "PoliceDept$", "PD"), "%m%d%Y"),
                  c(str_replace(co, "SheriffsOffice$", "SO"), "%m%d%Y"), c(str_replace(dept, "PoliceDept$", "PD"), "%m%d%y"),
                  c(str_replace(co, "SheriffsOffice$", "SO"), "%m%d%y"), c(str_replace(dept, "SheriffsDept$", "SD"), "%m%d%Y"),
                  c(dept, "%m.%d.%Y"), c(co, "%m%d%Y"))
    map_chr(forms, \(f) paste0(f[1], st, "_", model, "_MOA_", format(signed, f[2]), ".pdf"))
  })
  unique(unlist(files))
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

# every manifest under a snapshot root, with its folder and each row's resolved path
snapshot_manifests <- function(root = "agreements") {
  m <- list.files(root, "^manifest\\.csv$", recursive = TRUE, full.names = TRUE) |>
    map(\(p) read_manifest(p) |> mutate(folder = dirname(p), .before = 1)) |>
    list_rbind()
  for (col in manifest_columns) if (!col %in% names(m)) m[[col]] <- NA_character_
  m |>
    mutate(
      path_now = if_else(file.exists(saved_path), saved_path, file.path(folder, basename(saved_path))),
      on_disk = file.exists(path_now)
    )
}

# the newest participating-agencies workbook by manifest note or header, never by filename
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

# url -> the bytes held for it, their validators and where a copy lives, from the manifests
moa_validator_map <- function(m = snapshot_manifests("agreements")) {
  kept <- m |>
    filter(on_disk) |>
    distinct(file_hash, .keep_all = TRUE) |>
    select(file_hash, on_disk_path = path_now)
  m |>
    filter(!is.na(url), url != "") |>
    # bytes held under a Wayback-prefixed or http:// url count as held
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

# a manifest row whose file dedupe removed records where the identical bytes live
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
