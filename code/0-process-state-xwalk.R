library(tidyverse)
library(arrow)

state_xwalk <- tibble(
  state_abbr = state.abb,
  state_full = state.name,
  state_fips = c(
    "01", "02", "04", "05", "06", "08", "09", "10", "12", "13",
    "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
    "25", "26", "27", "28", "29", "30", "31", "32", "33", "34",
    "35", "36", "37", "38", "39", "40", "41", "42", "44", "45",
    "46", "47", "48", "49", "50", "51", "53", "54", "55", "56"
  )
) |>
  bind_rows(
    tibble(
      state_abbr = c("DC", "AS", "GU", "MP", "PR", "VI"),
      state_full = c(
        "District Of Columbia",
        "American Samoa",
        "Guam",
        "Commonwealth of the Northern Mariana Islands",
        "Puerto Rico",
        "United States Virgin Islands"
      ),
      state_fips = c("11", "60", "66", "69", "72", "78")
    )
  )

arrow::write_parquet(state_xwalk, "data/state_xwalk.parquet")
