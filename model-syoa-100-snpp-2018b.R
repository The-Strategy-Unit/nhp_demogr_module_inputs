
# README ----
# SNPP (2018-based) stop at age 90+
# NPP (2018-based) stop at age 104 (105-109, 110+)
# We take the 90+ distribution from NPP and apply it to SNPP to obtain a dataset
# for ages 0-100+
# Decision: which NPP variant should we take the 90+ distribution come from?
# Ideally, there exists a 1:1 mapping of SNPP variants to NPP variants,
# however no mapping exists, so we need to create one


# packages ----
{
  library("dplyr")
  library("here")
  library("purrr")
  library("readr")
  library("stringr")
  library("testthat")
  library("tidyr")
}


# 1 ----
apply_90plus_dist <- function(npp, snpp) {

  npp_dat <- read_rds(here("data", npp))
  snpp_dat <- read_rds(here("data", snpp))
  lookup_proj_var <- read_csv(here("data", "lookup_proj_var.csv"))

  # extract 90+ distribution from npp
  npp_plus90 <- npp_dat |>
    dplyr::group_by(id) |>
    tidyr::nest() |>
    dplyr::mutate(
      npp_p90 = purrr::map(
        data, ~ .x |>
          dplyr::filter(stringr::str_detect(age, "^1[0-1][0-9]|^9[0-9]")) |>
          dplyr::mutate(
            age =
              case_when(
                age %in% c(
                  "100", "101", "102", "103", "104",
                  "105 - 109", "110 and over"
                ) ~ "100",
                TRUE ~ as.character(age)
              )
          ) |>
          dplyr::group_by(dplyr::across(-c(pop))) |>
          dplyr::summarise(pop = sum(pop)) |>
          dplyr::group_by(dplyr::across(-c(age, pop))) |>
          dplyr::mutate(pop_pct = pop / sum(pop)) |>
          dplyr::ungroup() |>
          dplyr::mutate(age = as.integer(age)) |>
          dplyr::select(year, sex, age, pop_pct)
      )
    ) |>
    dplyr::filter(id %in% unique(var_map$map_id)) |>
    dplyr::select(-data) |> 
    dplyr::rename(map_id = id)
  
  # apply npp 90+ distribution to snpp
  out_dat <- snpp_dat |>
    dplyr::rename(ons_id = id) |>
    dplyr::group_by(ons_id) |>
    tidyr::nest() |>
    dplyr::mutate(snpp_p90 = purrr::map(data, ~ .x |>
                                          dplyr::filter(age_group == "90 and over") |>
                                          dplyr::mutate(age = 90L) %>%
                                          dplyr::select(-age_group) |>
                                          dplyr::group_by(area_code, area_name, sex, year) |>
                                          tidyr::complete(age = 90:100))) |>
    dplyr::left_join(var_map, by = "ons_id") |>
    dplyr::left_join(npp_plus90, by = "map_id") |>
    dplyr::mutate(snpp_p90 = map2(
      snpp_p90, npp_p90, ~ .x |>
        dplyr::left_join(.y, by = c("year", "sex", "age")) |>
        dplyr::group_by(area_code, area_name, sex, year) |>
        dplyr::mutate(pop = pop_pct * sum(pop, na.rm = TRUE)) |>
        dplyr::ungroup() |>
        dplyr::select(-pop_pct)
    )) |>
    dplyr::select(-npp_p90) |>
    dplyr::ungroup() |>
    dplyr::mutate(data = purrr::map(
      data, ~ .x |>
        dplyr::filter(!age_group == "90 and over") |>
        dplyr::rename(age = age_group) |>
        mutate(age = as.integer(age))
    )) |>
    dplyr::mutate(data = purrr::map2(
      data, snpp_p90, ~ dplyr::bind_rows(.x, .y)
    )) |>
    dplyr::select(-snpp_p90)
    
    # run some tests
    # source() code is evaluated in the global environment by default
    # set local = TRUE to evaluate the code in the calling environment
    source(here("tests", "testthat", "test-model-syoa-100-snpp-2018b.R"), local = TRUE)
    
    # write out
    readr::write_rds(out_dat, here::here("data", "snpp_2018b_syoa_100.rds"))
}

apply_90plus_dist("npp_2018b.rds", "snpp_2018b.rds")

