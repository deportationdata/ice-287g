library(tidyverse)
library(arrow)

state_xwalk <- tibble(
  state_abbr = state.abb,
  state_full = state.name
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
      )
    )
  )

arrow::write_parquet(state_xwalk, "data/processed/state_xwalk.parquet")
