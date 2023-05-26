
# README ----
# Read in life tables from national population projections (2018-based)
# x2 variant life tables were published alongside the principal


# packages ----
{
  library("dplyr")
  library("here")
  library("purrr")
  library("readxl")
  library("stringr")
  library("tidyr")
}


# read life tables 2018b ----
read_lt18_files <- function(dir) {
  lt18_files <- list.files(
    here::here(
      dir
    ),
    "18ex(.xls)$",
    recursive = TRUE,
    full.names = TRUE
  ) |>
    as.list()
  
  # obtain req sheet names
  ex18_sheets <- purrr::map(lt18_files, ~ readxl::excel_sheets(path = .x)) |>
    purrr::map(purrr::keep, stringr::str_detect, "period|cohort")
  id <- stringr::str_extract(lt18_files, "[a-z]{3}(?=18ex)")
  # iterate over files
  l <- purrr::map(lt18_files, function(x) {
    readxl::excel_sheets(x) |>
      stringr::str_subset(pattern = "period|cohort") |>
      # iterate over sheets
      purrr::map(function(y) {
        readxl::read_xls(x, y, skip = 9) |>
          dplyr::filter(!dplyr::row_number() == 1L) |>
          dplyr::rename_with(.cols = 1, ~"age") |>
          dplyr::mutate(base = "2018b") |>
          tidyr::pivot_longer(cols = `1981`:`2068`, names_to = "year", values_to = "ex") |>
          dplyr::mutate(sex = tolower(stringr::str_extract(y, "Females|Males")), type = stringr::str_extract(y, "period|cohort"), year = as.integer(year))
      }) |>
      dplyr::bind_rows()
  })
  l <- setNames(l, id)
  df <- purrr::map_df(l, ~ tibble::as_tibble(.x), .id = "var") |> 
    dplyr::mutate(sex = str_sub(sex, 1L, 1L)) |> 
    dplyr::group_by(type, var, base) |> 
    tidyr::nest() |> 
    dplyr::ungroup() |> 
    dplyr::select(type, var, base, data) |> 
    dplyr::arrange(type, var) |> 
    saveRDS(here::here("data", "npp_2018b_ex.rds"))
}

read_lt18_files("data-raw")
