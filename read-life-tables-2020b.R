
# README ----
# Read in life tables from national population projections (2020-based)
# principal only projection published


# packages ----
{
  library("dplyr")
  library("here")
  library("purrr")
  library("readxl")
  library("stringr")
  library("tidyr")
}


# read life tables 2020b ----
read_lt20_files <- function(dir) {
  lt20_files <- list.files(
    here::here(
      dir
    ),
    "20ex(.xlsx)$",
    recursive = TRUE,
    full.names = TRUE
  )
  # obtain req sheet names
  ex20_sheets <- purrr::map(lt20_files, ~ readxl::excel_sheets(path = .x)) |>
    purrr::map(purrr::keep, stringr::str_detect, "period|cohort")
  l <- readxl::excel_sheets(lt20_files) |>
    stringr::str_subset(pattern = "period|cohort") |>
    # iterate over sheets
    purrr::map(function(x) {
      readxl::read_xlsx(lt20_files, x, skip = 4) |>
        dplyr::mutate(base = "2020b") |>
        tidyr::pivot_longer(cols = `1981`:`2070`, names_to = "year", values_to = "ex") |>
        dplyr::mutate(sex = tolower(stringr::str_extract(x, "females|males")), type = stringr::str_extract(x, "period|cohort"), year = as.integer(year))
    }) |>
    dplyr::bind_rows() |> 
    dplyr::mutate(sex = stringr::str_sub(sex, 1L, 1L)) |> 
    saveRDS(here::here("data", "npp_2020b_ex.rds"))
}

read_lt20_files("data-raw")
