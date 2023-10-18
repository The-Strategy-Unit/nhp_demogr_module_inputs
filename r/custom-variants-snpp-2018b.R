# README ----
# x4 variant projections were published for SNPP (2018-based)
# x17 variant projections were published for NPP (2018-based)
# We create a set of x17 custom SNPP variants by applying the percentage
# difference between NPP variants (by age/year) and the NPP principal to the
# SNPP baseline


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
custom_vars_snpp <- function(npp, snpp) {

  npp_dat <- read_rds(here("data", npp))
  snpp_dat <- read_rds(here("data", snpp))

  ref <- npp_dat |>
    mutate(data = map(
      data, \(x) x |>
        # snpp runs to 2043
        filter(year %in% as.character(2018:2043)) |>
        mutate(age_group = case_when(
          age_group %in% c(as.character(100:104), "105 - 109", "110 and over")
          ~ "100", TRUE ~ age_group
        ),
        age_group = as.integer(age_group)) |>
        rename(age = age_group) |>
        # re-sum by age
        group_by(sex, age, year) |>
        summarise(pop = sum(pop)) |>
        ungroup()
    )) |>
    unnest(c(data))

  mx <- ref |>
    left_join(
      ref |>
        filter(ons_id == "npp_ppp") |>
        rename(npp_ppp = pop) |>
        select(-ons_id),
      join_by("year", "sex", "age")
    ) |>
    mutate(mx = pop / npp_ppp) |>
    filter(ons_id != "npp_ppp") |>
    select(-npp_ppp, -pop)

  custom <- map(
    mx |> group_split(ons_id), \(x) x |>
      left_join(
        snpp_dat |>
          filter(ons_id == "principal_proj") |>
          select(-ons_id) |>
          unnest(data),
        join_by("year", "sex", "age"), multiple = "all"
      ) |>
      mutate(new_pop = mx * pop, .before = pop) |>
      select(-pop, -mx) |>
      rename(pop = new_pop)
  ) |>
    list_rbind() |>
    nest(data = c(area_code, area_name, year, sex, age, pop))

  # run some tests
  lookup_proj_var <- read_csv(here("data", "lookup_proj_var.csv"))
  source(
    here("tests", "testthat", "test-custom-variants-snpp-2018b.R"),
    local = TRUE
  )

  # save
  write_rds(custom, here("data", "snpp_2018b_custom_vars.rds"))
}

custom_vars_snpp("npp_2018b.rds", "snpp_2018b_syoa_100.rds")