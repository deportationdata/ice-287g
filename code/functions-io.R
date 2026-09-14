# Reading: xlsx hyperlinks, ICE roster sheets and archived pages.

xlsx_hyperlinks <- function(path, sheet = 1) {
  files <- sprintf(
    c("xl/worksheets/sheet%d.xml", "xl/worksheets/_rels/sheet%d.xml.rels"),
    sheet
  )
  tmp <- tempfile()
  unzip(path, files = files, exdir = tmp)

  cells <- xml2::xml_find_all(
    xml2::read_xml(file.path(tmp, files[1])),
    ".//d1:hyperlink"
  )
  rels <- xml2::xml_find_all(
    xml2::read_xml(file.path(tmp, files[2])),
    ".//d1:Relationship"
  )

  tibble(
    ref = xml2::xml_attr(cells, "ref"),
    id = xml2::xml_attr(cells, "id")
  ) |>
    left_join(
      tibble(
        id = xml2::xml_attr(rels, "Id"),
        url = xml2::xml_attr(rels, "Target")
      ),
      by = "id"
    ) |>
    # an internal-anchor link has no relationship id and no external url
    filter(!is.na(url)) |>
    # a ref can be a range ("G5:G6"); take every row it covers, or the url
    # lands on the wrong agreement
    mutate(
      col = str_extract(ref, "^[A-Z]+"),
      row_start = as.integer(str_match(ref, "^[A-Z]+(\\d+)")[, 2]),
      row_end = as.integer(str_match(ref, ":[A-Z]+(\\d+)$")[, 2]),
      row_end = coalesce(row_end, row_start)
    ) |>
    mutate(row = map2(row_start, row_end, seq)) |>
    unnest(row) |>
    transmute(col, row = as.integer(row), url)
}

# the roster table an archived ICE page carries, in the participating-agencies
# workbook's column names, or NULL when the page has none (before April 2008 the
# page described the program in prose). Some eras split off a second table of
# agencies without trained officers; every table with the roster columns is kept
read_roster_html <- function(path) {
  # ICE's 2024-12-10 page had every "alt" replaced by "aria-label", in cell text and hrefs
  # alike ("Waria-labelon County Sheriff's Office"); the string occurs nowhere legitimately
  raw <- tryCatch(readr::read_file(path), error = function(e) NULL)
  if (is.null(raw)) return(NULL)
  html <- tryCatch(xml2::read_html(str_replace_all(raw, "aria-label", "alt")), error = function(e) NULL)
  if (is.null(html)) return(NULL)
  parsed <- map(as.list(rvest::html_elements(html, "table")), function(tb) {
    # cell by cell with html_text2, which keeps the space a <br> stands for
    # ("Sheriff's Office<br>Addendum"); html_table would glue the words
    rows <- keep(as.list(xml2::xml_find_all(tb, "./tr | ./thead/tr | ./tbody/tr | ./tfoot/tr")),
                        \(r) length(xml2::xml_find_all(r, "./td | ./th")) > 0)
    if (length(rows) < 2) return(NULL)
    cells <- map(rows, \(r) str_squish(rvest::html_text2(xml2::xml_find_all(r, "./td | ./th"))))
    header <- str_to_upper(cells[[1]])
    header[header == ""] <- paste0("X", seq_len(sum(header == "")))
    body <- keep(cells[-1], \(v) length(v) >= length(header))
    if (!length(body)) return(NULL)
    t <- map(body, \(v) tibble::as_tibble_row(setNames(v[seq_along(header)], make.unique(header)))) |>
      bind_rows()
    body_rows <- rows[-1][map_lgl(cells[-1], \(v) length(v) >= length(header))]
    names(t)[names(t) == "DATES SIGNED"] <- "SIGNED"
    # pre-2018 fact sheets: MOA NAME / OFFICIAL APPROVAL headers, state abbreviations, TFO/JEO
    legacy <- any(c("MOA NAME", "OFFICIAL APPROVAL") %in% names(t))
    names(t)[names(t) == "MOA NAME"] <- "LAW ENFORCEMENT AGENCY"
    names(t)[names(t) == "OFFICIAL APPROVAL"] <- "SIGNED"
    if (legacy && !"LAW ENFORCEMENT AGENCY" %in% names(t)) names(t)[names(t) == "AGENCY"] <- "LAW ENFORCEMENT AGENCY"
    if (legacy && !"SUPPORT TYPE" %in% names(t)) names(t)[names(t) == "TYPE"] <- "SUPPORT TYPE"
    if (!all(c("LAW ENFORCEMENT AGENCY", "SIGNED") %in% names(t))) return(NULL)
    names(t) <- make.unique(names(t))
    if (legacy && all(c("STATE", "SUPPORT TYPE") %in% names(t))) {
      ab <- c(setNames(state.name, state.abb), DC = "District of Columbia")
      st <- str_remove_all(str_squish(str_to_upper(t$STATE)), "[*†]+$")
      t$STATE <- coalesce(ab[st], t$STATE)
      ty <- str_squish(str_to_upper(t$`SUPPORT TYPE`))
      t$`SUPPORT TYPE` <- case_when(
        ty == "TFO" ~ "TASK FORCE", ty == "JEO" ~ "JAIL ENFORCEMENT",
        str_detect(ty, "JEO") & str_detect(ty, "TFO") ~ "JAIL & TASK FORCE",
        TRUE ~ t$`SUPPORT TYPE`)
    }
    # the MOA cell links the signed agreement: the href, made absolute, else the
    # cell's own text ("link pending")
    href <- map_chr(body_rows, \(r) {
      a <- xml2::xml_find_first(r, "./td//a[@href]")
      if (inherits(a, "xml_missing")) NA_character_ else xml2::xml_attr(a, "href")
    })
    href <- case_when(is.na(href) | href == "" ~ NA_character_,
                      str_starts(href, "/web/") ~ paste0("https://web.archive.org", href),
                      str_starts(href, "/") ~ paste0("https://www.ice.gov", href),
                      TRUE ~ href)
    if (any(!is.na(href))) t$MOA <- coalesce(href, if ("MOA" %in% names(t)) na_if(t$MOA, "") else NA_character_)
    # footnote markers on cells ("DELAWARE**")
    t |>
      mutate(across(everything(), ~ str_remove_all(str_squish(coalesce(.x, "")), "[*†]+$"))) |>
      filter(`LAW ENFORCEMENT AGENCY` != "")
  }) |> compact()
  if (!length(parsed)) return(NULL)
  bind_rows(parsed)
}

# the MOA / ADDENDUM urls live in a workbook's embedded hyperlinks, not its cells;
# a workbook without an ADDENDUM column, or without links, still yields both columns
workbook_links <- function(path) {
  header <- names(readxl::read_excel(path, n_max = 0)) |> str_squish() |> str_to_upper()
  link_cols <- c(moa_link = LETTERS[match("MOA", header)], addendum_link = LETTERS[match("ADDENDUM", header)])
  tryCatch(xlsx_hyperlinks(path), error = \(e) tibble::tibble(col = character(), row = integer(), url = character())) |>
    filter(col %in% link_cols) |>
    transmute(sheet_row = row, kind = names(link_cols)[match(col, link_cols)], url = clean_moa_urls(url)) |>
    distinct(sheet_row, kind, .keep_all = TRUE) |>
    tidyr::pivot_wider(names_from = kind, values_from = url) |>
    bind_rows(tibble::tibble(sheet_row = integer(), moa_link = character(), addendum_link = character()))
}

read_workbook <- function(path) {
  tabs <- readxl::excel_sheets(path)
  tab <- grep("^data$|^sheet", tabs, ignore.case = TRUE, value = TRUE)[1]
  if (is.na(tab)) tab <- tabs[1]
  d <- readxl::read_excel(path, sheet = tab, col_types = "text", progress = FALSE)
  names(d) <- str_squish(str_to_upper(names(d)))
  d
}

# one row per roster line of a workbook or an archived page, keeping the row
# number and the raw signed cell; rows with no parseable signing date are kept
# (flagged upstream), not dropped. NULL when there are no roster columns; the
# caller records it. `table` lets a caller pass a page it has already parsed
read_sheet_rows <- function(path, table = NULL) {
  tryCatch(
    {
      d <- if (!is.null(table)) table else if (str_detect(path, "\\.html?$")) read_roster_html(path) else read_workbook(path)
      need <- c("STATE", "LAW ENFORCEMENT AGENCY", "SUPPORT TYPE", "SIGNED")
      if (is.null(d) || !all(need %in% names(d))) return(NULL)
      opt <- function(col) if (col %in% names(d)) as.character(d[[col]]) else NA_character_
      tibble(
        sheet_row = seq_len(nrow(d)) + 1L,
        raw_state = as.character(d[["STATE"]]),
        raw_agency = as.character(d[["LAW ENFORCEMENT AGENCY"]]),
        raw_support = as.character(d[["SUPPORT TYPE"]]),
        raw_type = opt("TYPE"),
        raw_county = opt("COUNTY"),
        raw_moa = opt("MOA"),
        signed_raw = as.character(d[["SIGNED"]]),
        signed = suppressWarnings(coerce_signed_date(d[["SIGNED"]]))
      ) |>
        filter(!is.na(raw_state), !is.na(raw_agency), raw_state != "", raw_agency != "")
    },
    error = function(e) NULL
  )
}
