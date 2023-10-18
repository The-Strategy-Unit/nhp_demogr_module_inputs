# README ----
# Read in national population projections (2018-based)
# births by age of mother (sheet in main population file)
# x17 variant projections were published alongside the principal


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readxl")
library("readr")
library("stringr")
library("tidyr")

# read npp 2018b births ----
read_npp_birth_files <- function(dir) {
  npp_files <- list.files(
    here(dir),
    "2018(.xls)$",
    recursive = TRUE,
    full.names = TRUE
  )
  id <- str_extract(npp_files, "(?<=npp_2018b/).*(?=_opendata)")
  l <- map(npp_files, ~ readxl::read_xls(.x, sheet = "Births")) |>
    setNames(id)
  bind_rows(l, .id = "ons_id") |>
    rename_all(tolower) |>
    pivot_longer(
      cols = `2018 - 2019`:`2117 - 2118`,
      names_to = "year",
      names_prefix = "[0-9]{4} - ",
      values_to = "bths"
    ) |>
    mutate(
      age = as.integer(age),
      area = "England",
      bths = as.double(bths),
      ons_id = str_replace(ons_id, "en_", "npp_")
    ) |>
    select(-sex) |>
    nest(data = c(everything(), -ons_id)) |>
    write_rds(here("data", "npp_2018b_births.rds"))
}

read_npp_birth_files("data-raw")