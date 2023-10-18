# README ----
# x4 variant projections were published for SNPP (2018-based)
# x17 variant projections were published for NPP (2018-based)
# We create a set of x17 custom SNPP variants - for births by age of mother - by
# applying the percentage difference between NPP variants (by age/year) and the
# NPP principal to the SNPP baseline


# packages ----
library("dplyr")
library("ggplot2") # used by test_that file
library("here")
library("purrr")
library("readr")
library("stringr")
library("tidyr")
library("testthat")
library("tidytext") # used by test_that file

# 1 ----
custom_vars_snpp_births <- function(npp, snpp) {

  npp_dat <- read_rds(here("data", npp))
  snpp_dat <- read_rds(here("data", snpp))

  # snpp splits f|m births; npp total births only
  # therefore need to sum snpp counts to match
  snpp_dat <- snpp_dat |>
    mutate(data = map(
      data, \(x) x |>
        select(-sex) |>
        group_by(across(c(everything(), -bths))) |>
        summarise(bths = sum(bths)) |>
        ungroup()
    ))

  ref <- npp_dat |>
    mutate(data = map(
      data, \(x) x |>
        # snpp runs to 2043
        filter(year %in% as.character(2019:2043)) |>
        # npp has ages 15-46; snpp has ages 15-44
        mutate(age = case_when(
          age %in% c(45L, 46L) ~ 44L, TRUE ~ age
        )) |>
        # re-sum by age
        group_by(age, year) |>
        summarise(bths = sum(bths)) |>
        ungroup()
    )) |>
    unnest(c(data))

  mx <- ref |>
    left_join(
      ref |>
        filter(ons_id == "npp_ppp") |>
        rename(npp_ppp = bths) |>
        select(-ons_id),
      join_by("year", "age")
    ) |>
    mutate(mx = bths / npp_ppp) |>
    filter(ons_id != "npp_ppp") |>
    select(-npp_ppp, -bths)

  custom <- map(
    mx |> group_split(ons_id), \(x) x |>
      left_join(
        snpp_dat |>
          filter(ons_id == "principal_proj") |>
          select(-ons_id) |>
          unnest(data),
        join_by("year", "age"), multiple = "all"
      ) |>
      mutate(new_bths = mx * bths, .before = bths) |>
      select(-bths, -mx) |>
      rename(bths = new_bths)
  ) |>
    list_rbind() |>
    nest(data = c(area_code, area_name, year, age, bths))

  # run some tests
  lookup_proj_var <- read_csv(here("data", "lookup_proj_var.csv"))
  source(
    here("tests", "testthat", "test-custom-variants-snpp-2018b-births.R"),
    local = TRUE
  )

  # save
  write_rds(custom, here("data", "snpp_2018b_custom_vars_births.rds"))
}

custom_vars_snpp_births("npp_2018b_births.rds", "snpp_2018b_births.rds")