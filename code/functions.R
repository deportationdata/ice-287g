norm_key <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("&", " and ") |>
    str_replace_all("\\bst\\.?\\b", "saint") |>
    str_replace_all("\\bpd\\b", " ") |>
    str_replace_all(
      "\\b(county|city|town|village|borough|township|municipality)\\b",
      " "
    ) |>
    str_replace_all(
      "\\b(police|dept|department|public|safety|office)\\b",
      " "
    ) |>
    str_replace_all("\\b(of|the|and|for)\\b", " ") |>
    str_replace_all("[^a-z0-9]", "") |>
    str_squish()
}

norm_state <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z]", "") |>
    str_squish()
}

norm_place <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all(
      "\\b(county|city|town|village|borough|township|municipality)\\b",
      " "
    ) |>
    str_replace_all("[^a-z0-9\\s]", " ") |>
    str_squish() |>
    str_replace_all("\\s+", " ")
}

extract_city_guess <- function(x) {
  s <- str_squish(x)
  s <- str_remove(s, regex("(?i)^\\s*city\\s+of\\s+"))
  s <- str_remove(
    s,
    regex(
      "(?i)\\b(police|pd|police dept\\.?|police department|department|dept|public safety|office)\\b.*$"
    )
  )
  s <- str_squish(s)
  s <- na_if(s, "")
  str_to_title(s)
}

extract_facility_guess <- function(x) {
  s <- str_squish(x)

  s <- str_replace(
    s,
    regex("(?i)^(.+?)\\s+Sheriff'?s\\s+Office$"),
    "\\1 Jail"
  )

  s <- str_replace(
    s,
    regex("(?i)^(.+?)\\s+Police\\s+Department$"),
    "\\1 City Jail"
  )

  s <- str_replace(
    s,
    regex(
      "(?i)^(.+?)\\s+Board\\s+of\\s+County\\s+Commissioners\\s*/?\\s*(Department\\s+of\\s+Corrections|Detention\\s+Facility|Corrections)?$"
    ),
    "\\1 Jail"
  )

  s <- str_replace(
    s,
    regex("(?i)^(.+?)\\s+Department\\s+of\\s+Corrections$"),
    "\\1 Department of Corrections"
  )

  s <- str_replace_all(
    s,
    regex("(?i)corrections department"),
    "Department of Corrections"
  )

  s |>
    str_squish() |>
    str_to_title()
}

extract_university_guess <- function(x) {
  s <- str_squish(x)

  s <- str_remove(
    s,
    regex("(?i)^\\s*(district\\s+)?board\\s+of\\s+trustees\\s+of\\s+")
  )

  s <- str_remove(
    s,
    regex("(?i)\\s+board\\s+of\\s+trustees\\s*$")
  )

  s <- str_remove(
    s,
    regex(
      "(?i)\\s+((campus\\s+)?police(\\s+department)?|pd|department\\s+of\\s+public\\s+safety|public\\s+safety|security)\\s*$"
    )
  )

  s |>
    str_remove(regex("(?i)^\\s*the\\s+")) |>
    str_squish() |>
    str_to_title()
}
