source(here::here("R/utils.R"))

rsm.calculate <- function(
    freq = 'daily',
    start_date = "2022-01-01",
    end_date = Sys.Date() + 1,

    param_price_baseline = .35,
    param_skatt_grunnrente = .37,
    param_skatt_selskap    = .22,
    param_prop_spot_eksponert = .70,
    param_prop_hushold = .34,
    param_prop_inntekt_faktisk = .9,
    param_prop_statkraft = .42,
    param_prop_kommfylk = .54,
    param_prop_private = .04,
    na.rm = F
) {
  if (freq == 'daily') {
    power.prod <- nordpool.production.daily() %>% filter(zone != 'NO')
    power.prices <- nordpool.prices.daily()
  } else if (freq == 'hourly') {
    power.prod <- nordpool.production.hourly() %>% filter(zone != 'NO')
    power.prices <- nordpool.prices.hourly()
  } else {
    stop(glue("freq = {freq} is not supported"))
  }

  regions <- power.prices %>% distinct(region, displayName) %>% rename(region.name = displayName)

  ssb_prop_vannkraft <- ssb.table("12824", ContentsCode = 'Kraft', Tid = T, Produk2 = c("01", "01.01")) %>%
    rename_with(tolower) %>%
    mutate(date = lubridate::ym(tid)) %>%
    pivot_wider(c(date, tid), names_from=produk2.label, values_from=value) %>%
    janitor::clean_names() %>%
    arrange(date) %>%
    transmute(
      date,
      ssb.prop.vannkraft = vannkraft / total_produksjon_av_elektrisk_kraft,
      ssb.prop.vannkraft.ma4 = slider::slide_dbl(., ~mean(.x$vannkraft)/mean(.x$total_produksjon_av_elektrisk_kraft), .before = 3, .complete = T)
    )

  dat.long <-
    bind_rows(
      power.prod %>%
        transmute(
          date,
          region = tolower(zone),
          value = MWh * 1000,
          variable = "pro"
        ),
      power.prices %>%
        transmute(date, region = tolower(region), value = value / 100, variable = "price")
    )

  dat.wide <- dat.long %>%
    pivot_wider(c(date, region), names_from = variable, values_from = value) %>%
    arrange(date) %>%
    left_join(ssb_prop_vannkraft, by = 'date') %>%
    fill(c(ssb.prop.vannkraft, ssb.prop.vannkraft.ma4), .direction = 'down')

  if (na.rm) {
    dat.wide <- dat.wide %>%
      filter(!(is.na(pro) | is.na(price)))
  }

  result_regional <- dat.wide %>%
    filter(
      date >= as.Date(start_date),
      date <= as.Date(end_date)
    ) %>%
    mutate(
      year_month = format(date, "%Y-%m"),
      quarter = lubridate::quarter(date),
    ) %>%
    group_by(region, year_month) %>%
    mutate(price_monthly_mean = mean(price, na.rm = T)) %>%
    ungroup() %>%
    mutate(
      param_mva = case_when(
        region == 'no4' ~ 0,
        T ~ .25
      ),

      param_stromstotte_floor = .70,
      param_stromstotte_prop  = case_when(
        date < "2021-12-01"                        ~ 0,
        date >= "2021-12-01" & date < "2022-01-01" ~ .55,
        date >= "2022-01-01" & date < "2022-09-01" ~ .8,
        date >= "2022-09-01"                       ~ .9
      ),

      # ikke i bruk
      param_elavgift_per_kwh = case_when(
        date >= "2021-01-01" & date < "2022-01-01" ~ .1669,
        date >= "2022-01-01" & date < "2022-04-01" ~ .0891,
        date >= "2022-04-01" ~ .1541,
      ),

      param_prop_hydropower = ssb.prop.vannkraft.ma4
    ) %>%
    mutate(
      alt_til_spotpris           = pro * price,
      stromstotte_per_kwh        = pmax(price_monthly_mean - param_stromstotte_floor, 0) * param_stromstotte_prop,
      en.vkraft                  = pro * param_prop_hydropower * param_prop_spot_eksponert,

      en.inntekt.normert.spot    = en.vkraft * price,
      en.inntekt.normert.normal  = en.vkraft * param_price_baseline,
      en.inntekt.normert.ekstra  = en.inntekt.normert.spot - en.inntekt.normert.normal,

      en.inntekt.faktisk.spot   = en.inntekt.normert.spot * param_prop_inntekt_faktisk,
      en.inntekt.faktisk.normal = en.inntekt.normert.normal * param_prop_inntekt_faktisk,

      en.grunnrenteskatt.spot   = en.inntekt.normert.spot * param_skatt_grunnrente,
      en.grunnrenteskatt.normal = en.inntekt.normert.normal * param_skatt_grunnrente,
      en.grunnrenteskatt.ekstra = en.grunnrenteskatt.spot - en.grunnrenteskatt.normal,

      en.selskapsskatt.spot     = en.inntekt.faktisk.spot * param_skatt_selskap,
      en.selskapsskatt.normal   = en.inntekt.faktisk.normal * param_skatt_selskap,
      en.selskapsskatt.ekstra   = en.selskapsskatt.spot - en.selskapsskatt.normal,

      en.skatt.spot             = en.selskapsskatt.spot + en.grunnrenteskatt.spot,
      en.skatt.normal           = en.selskapsskatt.normal + en.grunnrenteskatt.normal,
      en.skatt.ekstra           = en.skatt.spot - en.skatt.normal,

      en.resultat_til_eiere.spot   = en.inntekt.faktisk.spot - en.skatt.spot,
      en.resultat_til_eiere.normal = en.inntekt.faktisk.normal - en.skatt.normal,
      en.resultat_til_eiere.ekstra = en.resultat_til_eiere.spot - en.resultat_til_eiere.normal,

      across(
        c(
          en.resultat_til_eiere.spot,
          en.resultat_til_eiere.normal,
          en.resultat_til_eiere.ekstra
        ),
        list(
          statkraft = ~ .x * param_prop_statkraft,
          kommfylk  = ~ .x * param_prop_kommfylk,
          private   = ~ .x * param_prop_private
        ),
        .names = "{.col}.{.fn}"
      ),

      en.hushold.vkraft           = en.vkraft * param_prop_hushold,
      en.hushold.stromstotte      = (en.hushold.vkraft * stromstotte_per_kwh),
      en.hushold.stromstotte.mmva = en.hushold.stromstotte * (1 + param_mva),

      en.hushold.kost.spot        = (en.hushold.vkraft * price) - en.hushold.stromstotte,
      en.hushold.kost.normal      = en.hushold.vkraft * param_price_baseline,
      en.hushold.kost.ekstra      = en.hushold.kost.spot - en.hushold.kost.normal,

      en.hushold.mva.spot   = en.hushold.kost.spot * param_mva,
      en.hushold.mva.normal = en.hushold.kost.normal * param_mva,
      en.hushold.mva.ekstra = en.hushold.kost.ekstra * param_mva,

      en.staten.spot   = en.skatt.spot + en.resultat_til_eiere.spot.statkraft + en.hushold.mva.spot,
      en.staten.normal = en.skatt.normal + en.resultat_til_eiere.normal.statkraft + en.hushold.mva.normal,
      en.staten.ekstra = en.skatt.ekstra + en.resultat_til_eiere.ekstra.statkraft + en.hushold.mva.ekstra,

      en.staten.ekstra_etter_stotte = en.staten.ekstra - en.hushold.stromstotte.mmva,
    ) %>%
    inner_join(regions, by = "region") %>%
    arrange(date) %>%
    group_by(region) %>%
    mutate(
      across(c(
        pro,
        alt_til_spotpris,
        starts_with("en.")
      ), ~ cumsum(.x), .names = "{.col}.cum")
    ) %>%
    ungroup()

  result_national <- result_regional %>%
    group_by(date, year_month, quarter) %>%
    summarise(
      across(-c(
        starts_with("param_"),
        region,
        region.name,
        price,
        price_monthly_mean,
        stromstotte_per_kwh,
        ssb.prop.vannkraft,
        ssb.prop.vannkraft.ma4
      ), ~ sum(.x, na.rm = T)),
      .groups = "drop"
    ) %>%
    arrange(date)

  bind_rows(
    result_regional,
    result_national %>% mutate(region = 'no', region.name = 'Norge')
  )
}
