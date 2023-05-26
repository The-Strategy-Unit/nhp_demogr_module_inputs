
# README ----
# x4 variant projections are published for sub-national population projections (2018-based)
# x17 variant projections are published for national population projections (2018-based)
# We create a set of x17 custom SNPP variants by applying the % difference between
# NPP variants (by age/sex/year) and the NPP principal to the SNPP baseline


# packages ----
{
  library("dplyr")
  library("here")
  library("purrr")
  library("readr")
  library("stringr")
  library("tidyr")
  library("testthat")
}


# 1 ----
custom_vars_snpp <- function(npp, snpp) {
  
  npp_dat <- readRDS(here::here("data", npp))
  snpp_dat <- readRDS(here::here("data", snpp))
  
  ref <- npp_dat |>
    dplyr::filter(year %in% as.character(2018:2043)) |>  # snpp end at 2043
    dplyr::mutate(age = dplyr::case_when(
      age %in% c(as.character(100:104), "105 - 109", "110 and over")
      ~ "100", TRUE ~ age
    )) |>
    dplyr::group_by(id, sex, age, year) |>  # re-sum by age
    dplyr::summarise(pop = sum(pop)) |>
    dplyr::ungroup() 
  
  mx <- ref |>
    dplyr::left_join(
      ref |> dplyr::filter(id == "en_ppp") |> dplyr::rename(en_ppp = pop) |> dplyr::select(-id),
      by = c("year", "sex", "age")
    ) |>
    dplyr::mutate(mx = pop / en_ppp, age = as.integer(age)) |>
    dplyr::ungroup() |>
    dplyr::filter(id != "en_ppp") |>
    dplyr::select(-en_ppp, -pop)
  
  custom <- purrr::map(mx |>
                         dplyr::group_split(id), ~ .x |>
                         dplyr::left_join(
                           snpp_dat |>
                             dplyr::filter(ons_id == "principal_proj") |>
                             dplyr::select(-ons_id) |>
                             tidyr::unnest(data),
                           by = c("year", "sex", "age"), multiple = "all"
                         ) |>
                         dplyr::mutate(new_pop = mx * pop, .before = pop) |>
                         dplyr::select(-pop, -mx) |>
                         dplyr::rename(pop = new_pop)
  ) |>
    purrr::list_rbind() |> 
    dplyr::mutate(ons_id = str_c("npp_", str_extract(id, "[a-z]{3}$"))) |> 
    tidyr::nest(data = c(area_code, area_name, year, sex, age, pop)) |> 
    dplyr::select(ons_id, id, data)
  
  # run some tests
  source(here("tests", "testthat", "test-custom-variants-snpp-2018b.R"), local = TRUE)
  
  # write out
  write_rds(custom, here::here("data", "snpp_2018b_custom_vars.rds"))
}

custom_vars_snpp("npp_2018b.rds", "snpp_2018b_syoa_100.rds")

