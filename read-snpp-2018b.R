
# README ----
# Read in sub-national population projections (2018-based)
# x4 variant projections were published alongside the principal


# packages ----
{
  library("dplyr")
  library("here")
  library("purrr")
  library("stringr")
  library("tidyr")
}


# read snpp 2018b ----
read_snpp_files <- function(dir) {
  snpp_files <- list.files(
    here::here(
      dir
    ),
    "^(2018 SNPP).*(females|males).*(.csv$)",
    recursive = TRUE,
    full.names = TRUE
  )
  id <- stringr::str_match(snpp_files, "2018b_(.*?)/2018")[, 2]
  l <- purrr::map(snpp_files, ~ readr::read_csv(.x)) |>
    setNames(id)
  dat <- dplyr::bind_rows(l, .id = "id") |>
    dplyr::rename_all(tolower) |>
    tidyr::pivot_longer(cols = `2018`:`2043`, names_to = "year", values_to = "pop") |>
    dplyr::filter(age_group != "All ages") |>
    dplyr::select(-component) |>
    dplyr::mutate(sex = str_sub(sex, 1L, 1L)) |>
    # remove metropolitan counties and regions (keep only local authorities)
    dplyr::filter(stringr::str_detect(area_code, "^E11|^E12", negate = TRUE)) |>
    saveRDS(here("data", "snpp_2018b.rds"))
}

read_snpp_files("data-raw")
