library(tidyr)
library(PxWebApiData)
library(lubridate)
library(glue)
library(here)
library(dotenv)
library(memoise)
library(stringr)

if (file.exists(here::here(".env"))) {
  load_dot_env()
}

api_url <- function(key) {
  val <- Sys.getenv(key)

  if (val == '') {
    stop(glue("Missing env var: {key}"))
  }

  val
}

nordpool.prices.daily <- memoise(function() {
  print("fetching nordpool.prices.daily")

  strom.historisk <- jsonlite::fromJSON(api_url('API_PRICES_DAILY'), flatten = T)

  priser <- strom.historisk$historic$prices %>%
    as_tibble() %>%
    mutate(
      date = list(as.Date(strom.historisk$historic$dates))
    ) %>%
    unnest(c(data, date)) %>%
    rename(value = data) %>%
    mutate(region = str_extract(displayName, "NO\\d+") %>% tolower())

  priser
})

nordpool.regions <- tibble::tribble(
  ~name,          ~displayName, ~region,
  "oslo",  "Sørøst-Norge (NO1)",   "no1",
  "kristiansand", "Sørvest-Norge (NO2)",   "no2",
  "bergen",    "Vest-Norge (NO5)",   "no5",
  "trondheim",    "Midt-Norge (NO3)",   "no3",
  "tromso",    "Nord-Norge (NO4)",   "no4"
)

nordpool.prices.hourly <- memoise(function() {
  print("fetching nordpool.prices.hourly")
  dat <- jsonlite::fromJSON(api_url('API_PRICES_HOURLY'), flatten = T)
  rows <- dat$rows

  colnames(rows) <- dat$columns

  rows %>% as_tibble() %>%
    separate(hours, into = c("hour.start", "hour.end")) %>%
    mutate(
      date = lubridate::ymd_h(str_c(date, hour.start, sep = " "))
    ) %>%
    pivot_longer(-c(date, hour.start, hour.end)) %>%
    left_join(nordpool.regions, by = 'name') %>%
    mutate(value = as.numeric(value))
})

nordpool.production.daily <- memoise(function() {
  print("fetching nordpool.production.daily")
  prod <- jsonlite::fromJSON(api_url('API_PROD_DAILY'), flatten = T)$items
  prod %>%
    as_tibble() %>%
    mutate(date = as.Date(date))
})

nordpool.production.hourly <- memoise(function() {
  print("fetching nordpool.production.hourly")

  prod <- jsonlite::fromJSON(api_url('API_PROD_HOURLY'), flatten = T)$items
  prod %>%
    as_tibble() %>%
    separate(hours, into = c("hour.start", "hour.end")) %>%
    mutate(
      date = lubridate::ymd_h(str_c(date, hour.start, sep = " "))
    )
})

ssb.table <- memoise(function(tbl, ...) {
  print("fetching SSB:{tbl}" %>% glue())
  args <- list(...)

  meta <- ApiData(tbl, returnMetaFrames = T)
  data <- do.call("ApiData", c(tbl, args))

  dat <- data$dataset %>% as_tibble()

  for (i in 1:length(meta)) {
    name <- names(meta[i])
    label_name <- str_c(name, ".Label")

    if (!(name %in% colnames(dat))) {
      next
    }

    to_join <- tibble(.rows = nrow(meta[[name]]))

    to_join[[name]] <- meta[[name]]$values
    to_join[[label_name]] <- meta[[name]]$valueTexts

    dat <- dat %>%
      inner_join(to_join, by = all_of(name)) %>%
      relocate(all_of(label_name), .after = all_of(name))
  }

  dat %>%
    rename(Variable = ContentsCode, Variable.Label = ContentsCode.Label, Value = value)
})

ssb.meta <- memoise(function(tbl, simplify = F) {
  dat <- ApiData(tbl, returnMetaFrames = T)

  if (!simplify) {
    return(dat)
  }

  names <- names(dat)
  result <- tibble()

  for (name in names) {
    result <- result %>% bind_rows(dat[[name]] %>% as_tibble() %>% transmute(
      key = name,
      code = values,
      desc = valueTexts
    ))
  }

  result
})
