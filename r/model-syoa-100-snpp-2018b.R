# README ----
# snpp (2018-based) ends at age 90+; npp (2018-based) ends at age 104,
# (105-109, 110+). We take the 90+ distribution from npp and apply it to snpp to
# obtain a dataset for ages 0-100+.
# Q. Which NPP variant should we take the 90+ distribution from?
# ideally, there exists a 1:1 mapping of snpp variants to npp variants,
# however no mapping exists, so we must create one (see create-lookups.R)


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")
library("stringr")
library("testthat")
library("tidyr")

# 1 ----
apply_90plus_dist <- function(npp, snpp) {

  npp_dat <- read_rds(here("data", npp))
  snpp_dat <- read_rds(here("data", snpp))
  lookup_proj_var <- read_csv(here("data", "lookup_proj_var.csv"))

  # extract 90+ distribution from npp
  npp_p90 <- npp_dat |>
    mutate(npp_p90 = map(
      data, \(x) x |>
        filter(str_detect(age_group, "^1[0-1][0-9]|^9[0-9]")) |>
        mutate(age_group = case_when(
          age_group %in% c("100", "101", "102", "103", "104",
            "105 - 109", "110 and over"
          ) ~ "100",
          TRUE ~ age_group
        ),
        age_group = as.integer(age_group)
        ) |>
        rename(age = age_group) |>
        group_by(across(-c(pop))) |>
        summarise(pop = sum(pop)) |>
        group_by(across(-c(age, pop))) |>
        mutate(pop_pct = pop / sum(pop)) |>
        ungroup() |>
        select(year, sex, age, pop_pct)
    )) |>
    filter(ons_id %in% c("npp_ppp", "npp_pph", "npp_ppl")) |>
    select(-data) |>
    left_join(
      lookup_proj_var |>
        select(ons_id, id),
      join_by("ons_id")
    )

  # apply npp 90+ distribution to snpp
  out_dat <- snpp_dat |>
    mutate(snpp_p90 = map(
      data, \(x) x |>
        filter(age_group == "90 and over") |>
        mutate(age_group = 90L) %>%
        rename(age = age_group) |>
        group_by(area_code, area_name, sex, year) |>
        tidyr::complete(age = 90:100) |>
        ungroup()
    )) |>
    left_join(
      lookup_proj_var |>
        select(ons_id, id),
      join_by("ons_id")
    ) |>
    left_join(npp_p90, join_by("id")) |>
    mutate(snpp_p90 = map2(
      snpp_p90, npp_p90, \(x, y) x |>
        left_join(y, join_by("year", "sex", "age")) |>
        group_by(area_code, area_name, sex, year) |>
        mutate(pop = pop_pct * sum(pop, na.rm = TRUE)) |>
        ungroup() |>
        select(-pop_pct)
    )) |>
    select(-npp_p90) |>
    ungroup() |>
    mutate(data = map(
      data, \(x) x |>
        filter(!age_group == "90 and over") |>
        rename(age = age_group) |>
        mutate(age = as.integer(age))
    )) |>
    mutate(data = map2(
      data, snpp_p90, \(x, y) bind_rows(x, y)
    ))

  # run some tests
  # source() code is evaluated in the global environment by default
  # set local = TRUE to evaluate the code in the calling environment
  source(here(
    "tests", "testthat", "test-model-syoa-100-snpp-2018b.R"
  ), local = TRUE)

  out_dat <- out_dat |>
    rename(ons_id = ons_id.x) |>
    select(-snpp_p90, -ons_id.y)

  # save
  write_rds(out_dat, here("data", "snpp_2018b_syoa_100.rds"))
}

apply_90plus_dist("npp_2018b.rds", "snpp_2018b.rds")