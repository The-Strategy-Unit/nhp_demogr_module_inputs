# README ----
# Read-in sub-national population projections (2018-based)
# x4 variant projections were published alongside the principal


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")
library("stringr")
library("tidyr")

# read snpp 2018b ----
read_snpp_files <- function(dir) {
  snpp_files <- list.files(
    here(dir),
    "^(2018 SNPP).*Population.*(females|males).*(.csv$)",
    recursive = TRUE,
    full.names = TRUE
  )
  id <- str_match(snpp_files, "2018b_(.*?)/2018")[, 2]
  l <- map(snpp_files, ~ read_csv(.x)) |>
    setNames(id)
  bind_rows(l, .id = "ons_id") |>
    rename_all(tolower) |>
    pivot_longer(cols = `2018`:`2043`, names_to = "year", values_to = "pop") |>
    filter(age_group != "All ages") |>
    select(-component) |>
    mutate(
      sex = str_sub(sex, 1L, 1L),
      ons_id = str_replace(ons_id, "en_", "npp_")
    ) |>
    # remove metropolitan counties and regions (keep only local authorities)
    filter(str_detect(area_code, "^E11|^E12", negate = TRUE)) |>
    nest(data = c(everything(), -ons_id)) |>
    write_rds(here("data", "snpp_2018b.rds"))
}

read_snpp_files("data-raw")