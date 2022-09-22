# Create custom sub-national projection variants
# NHP model - demographic module (942)
# author Paul Seamer


# README ----
# x4 variant projections are published as part of the 2018b SNPP
# x17 variant projections are published as part of the 2018b NPP
# this script creates a set of x17 custom sub-national projection variants based
# on the age/sex/year growth rates used in the NPP variants


# in this script ----
# 1 read
# 2 create custom vars
# 3 tests
# 4 save


# packages ----
library("tidyverse")
library("here")
library("testthat")

# pull multipliers
pull_mx <- function(npp_proj_var_df) {
  npp_proj_var_df |> 
    filter(year %in% as.character(2018:2043)) |> # snpp ends at 2043
    mutate(age = case_when(
      age %in% c(as.character(100:104), "105 - 109", "110 and over")
      ~ "100", TRUE ~ age
    )) |> 
    group_by(sex, age, year) |> # re-sum by age
    summarise(pop = sum(pop)) |> 
    ungroup() |> 
    mutate(age = as.integer(age)) |> 
    arrange(sex, age, year) |>  # sort by year
    group_by(sex, age) |> # mx for sex/age groups
    mutate(mx = pop / lag(pop, n = 1L)) |> 
    ungroup() |> 
    select(year, sex, age, mx)
}

# apply multipliers
app_mx <- function(mx, snpp_principal) {
  snpp_principal |> 
    group_by(area_code, area_name) |> 
    arrange(sex, age, year, .by_group = TRUE) |>  # sort by year within group
    ungroup() |> 
    left_join(mx, by = c("sex", "age", "year")) |> 
    mutate(mx = ifelse(is.na(mx), pop, mx)) |> # base year pop
    group_by(area_code, area_name, sex, age) |> # mx for sex/age groups within areas
    mutate(new_pop = cumprod(mx)) |> 
    ungroup() |> 
    select(-pop, -mx) |> 
    rename(pop = new_pop)
}




# 1 read ----
nhp_snpp_2018b <- readRDS(here("data","nhp_snpp_2018b.rds"))
npp_2018b_dat  <- readRDS(here("data","npp_2018b_dat.rds"))




# 2 create custom variants ----
# pull multipliers
ls_mx <- map(npp_2018b_dat, pull_mx)
ls_mx <- imap(ls_mx, ~ .x |> mutate(id = .y))
# apply multipliers
ls_snpp_custom_vars <- map(ls_mx, ~ app_mx(., nhp_snpp_2018b$principal_proj |> select(-id)))




# 3 tests ----
source(here("tests", "testthat", "test_002_custom_snpp_vars.R"))




# 4 save ----
saveRDS(ls_snpp_custom_vars, here("data","nhp_snpp_2018b_custom_vars.rds"))

