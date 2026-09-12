# Collapse the East et al. (2023) county-month panel into 287(g) spells -> data/pre2018-east-county-spells.csv.

suppressPackageStartupMessages({ library(haven); library(dplyr); library(stringr) })

ROOT <- "."  # run from the repo root
DTA  <- file.path(ROOT, "inputs", "pre2018", "panels", "287g_SC_EVerify_5_13_22.dta")
OUT  <- file.path(ROOT, "data"); dir.create(OUT, showWarnings = FALSE)
if (!file.exists(DTA)) stop("Missing ", DTA, " -- run code/0-287g-pre2018-download.R")

p <- read_dta(DTA) |>
  mutate(across(everything(), as.numeric),
         fips = statefip * 1000 + countyfip,
         t    = year * 12 + month,
         local_287g = as.integer(jail287g > 0 | task287g > 0),
         model = case_when(jail287g > 0 & task287g > 0 ~ "Hybrid",
                           jail287g > 0               ~ "Jail Enforcement",
                           task287g > 0               ~ "Task Force",
                           TRUE                       ~ NA_character_))

cat(sprintf("Panel: %d county-months, %d counties, %d-%d\n",
            nrow(p), n_distinct(p$fips), min(p$year), max(p$year)))

ym <- function(t) { y <- (t - 1) %/% 12; m <- t - 12 * y; sprintf("%d-%02d", y, m) }

spells <- p |>
  filter(local_287g == 1) |>
  arrange(fips, t) |>
  group_by(fips) |>
  mutate(gap = t - lag(t, default = first(t) - 1) != 1,
         spell = cumsum(gap)) |>
  group_by(fips, spell) |>
  summarise(start_ym = ym(min(t)), end_ym = ym(max(t)),
            end_t = max(t),
            models = paste(sort(unique(na.omit(model))), collapse = " / "),
            n_months = n(), .groups = "drop") |>
  mutate(right_censored = end_t >= max(p$year[p$local_287g == 1], na.rm = TRUE) * 12 + 12,
         source = "East et al. 2023 (JOLE) replication panel") |>
  select(-end_t, -spell)

state_only <- p |> filter(state287g > 0, local_287g == 0) |>
  distinct(statefip) |> pull(statefip)

write.csv(spells, file.path(OUT, "pre2018-east-county-spells.csv"), row.names = FALSE, na = "")
cat(sprintf("%d local 287(g) spells across %d counties; %d ended before the panel does\n",
            nrow(spells), n_distinct(spells$fips), sum(!spells$right_censored)))
cat(sprintf("States with state-level 287(g) exposure coded: %s\n",
            paste(sort(state_only), collapse = ", ")))
