rm(list = ls())

library(tidyverse)
library(purrr)
library(dplyr)
library(readr)


source(here::here("R/main.R"))

params <- expand_grid(
  freq = c("hourly"),
  start_date = c("2022-01-01"),
  # end_date = c("2022-07-31"),
  param_price_baseline = c(.25, .35, .4),
  param_skatt_grunnrente = .37,
  param_skatt_selskap = .22,
  param_prop_spot_eksponert = c(.5, .7),
  param_prop_hushold = .34,
  param_prop_inntekt_faktisk = .9,
  param_prop_statkraft = .42,
  param_prop_kommfylk = .54,
  param_prop_private = .04,
  na.rm = T
)

params

result <- pmap_dfr(
  params,
  function(...) {
    rsm.calculate(...) %>% mutate(...)
  },
  .id = "group"
)

minimize <- function(dat) {
  dat %>%
    select(
      where(
        ~ sum(!is.na(.x)) > 0
      )
    ) %>%
    select(
      group,
      freq,
      date,
      starts_with("param_"),
      en.staten.spot,
      en.staten.normal,
      en.staten.ekstra,
      en.staten.spot.cum,
      en.staten.normal.cum,
      en.staten.ekstra.cum,
    )
}

national <- result %>% filter(region == "no")

weekly <- national %>%
  mutate(week_start = lubridate::floor_date(as.Date(date), "week", week_start = 1)) %>%
  group_by(group, week_start, across(starts_with("param_"))) %>%
  summarise(
    across(c(
      en.staten.spot,
      en.staten.normal,
      en.staten.ekstra,
    ), ~ sum(.x, na.rm = T)),
    .groups = "drop"
  ) %>%
  rename(date = week_start) %>%
  group_by(group) %>%
  arrange(date) %>%
  mutate(
    freq = "weekly",
    across(c(en.staten.spot, en.staten.normal, en.staten.ekstra), cumsum, .names = "{.col}.cum")
  ) %>%
  ungroup() %>%
  minimize()

weekly

latest <- national %>%
  filter(date >= Sys.Date() - 1) %>%
  minimize()

minimal <- bind_rows(weekly, latest)

dir.create(here::here("outputs"), showWarnings = F)
max_date <- format(max(result$date), "%Y-%m-%d-%H%M%S")
max_date

write_csv(national, here::here(glue("outputs/merinntekter-no-{max_date}")))
write_csv(minimal, here::here(glue("outputs/merinntekter-minimal-latest.csv")))
write_file(minimal %>% jsonlite::toJSON(), here::here(glue("outputs/merinntekter-minimal-latest.json")))
