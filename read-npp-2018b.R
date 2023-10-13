# README ----
# Read-in national population projections (2018-based)
# x17 variant projections were published alongside the principal


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")
library("readxl")
library("stringr")
library("tidyr")

# read npp 2018b ----
change_file_ext <- function(x) {
  system2("powershell", args = c("-file", x))
}
# only need to run this once to change file extension
# change_file_ext("cmd-line-dark-arts.ps1")

read_npp_files <- function(dir) {
  npp_files <- list.files(
    here(dir),
    "2018(.xls)$",
    recursive = TRUE,
    full.names = TRUE
  )
  id <- str_extract(npp_files, "(?<=npp_2018b/).*(?=_opendata)")
  l <- map(npp_files, ~ read_xls(.x, sheet = "Population")) |>
    setNames(id)
  dat <- bind_rows(l, .id = "ons_id") |>
    rename_all(tolower) |>
    pivot_longer(cols = `2018`:`2118`, names_to = "year", values_to = "pop") |>
    mutate(
      age = str_trim(age),
      area = "England",
      pop = as.double(pop),
      sex = ifelse(sex == "1", "m", "f"),
      ons_id = str_replace(ons_id, "en_", "npp_")
    ) |>
    rename(age_group = age) |>
    nest(data = c(everything(), -ons_id)) |>
    write_rds(here("data", "npp_2018b.rds"))
}

read_npp_files("data-raw")