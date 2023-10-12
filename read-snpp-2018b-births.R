# README ----
# Read in sub-national population projections (2018-based)
# births by age of mother (SNPP Z3)
# x4 variant projections were published alongside the principal


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")
library("stringr")
library("tidyr")

# read snpp 2018b births ----
read_snpp_birth_files <- function(dir) {
  snpp_files <- list.files(
    here(dir),
    "^(2018 SNPP).*(Births).*(females|males).*(.csv$)",
    recursive = TRUE,
    full.names = TRUE
  )
  id <- str_match(snpp_files, "2018b_(.*?)/2018")[, 2]
  l <- map(snpp_files, ~ read_csv(.x)) |>
    setNames(id)
  bind_rows(l, .id = "id") |>
    rename_all(tolower) |>
    # first year of births is 2019
    pivot_longer(cols = `2019`:`2043`, names_to = "year", values_to = "bths") |>
    filter(age_group != "All ages") |>
    rename(age = age_group) |>
    select(-component) |>
    mutate(sex = str_sub(sex, 1L, 1L)) |>
    # remove metropolitan counties and regions (keep only local authorities)
    filter(str_detect(area_code, "^E11|^E12", negate = TRUE)) |>
    write_rds(here("data", "snpp_2018b_births.rds"))
}

read_snpp_birth_files("data-raw")