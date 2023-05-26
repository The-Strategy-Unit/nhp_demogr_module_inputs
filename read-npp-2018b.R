
# README ----
# Read in national population projections (2018-based)
# x17 variant projections were published alongside the principal


# packages ----
{
  library("dplyr")
  library("here")
  library("purrr")
  library("readxl")
  library("stringr")
  library("tidyr")
}


# read npp 2018b ----
change_file_ext <- function(x) {
  system2("powershell", args = c("-file", x))
}
# only need to run this once to change file extension
# change_file_ext("cmd_line_shithousery.ps1")

read_npp_files <- function(dir) {
  npp_files <- list.files(
    here::here(
      dir
    ),
    "2018(.xls)$",
    recursive = TRUE,
    full.names = TRUE
  )
  id <- stringr::str_extract(npp_files, "(?<=npp_2018b/).*(?=_opendata)")
  l <- purrr::map(npp_files, ~ readxl::read_xls(.x, sheet = "Population")) |>
    setNames(id)
  dat <- dplyr::bind_rows(l, .id = "id") |>
    dplyr::rename_all(tolower) |>
    tidyr::pivot_longer(cols = `2018`:`2118`, names_to = "year", values_to = "pop") |>
    dplyr::mutate(age = stringr::str_trim(age), area = "England", pop = as.double(pop)) |>
    dplyr::mutate(sex = ifelse(sex == "1", "m", "f")) |> 
    saveRDS(here("data", "npp_2018b.rds"))
}

read_npp_files("data-raw")

# variant codes
read_npp_codes <- function(dir) {
  proj_codes_file <- list.files(
    here::here(
      dir
    ),
    "NPP codes.txt",
    recursive = TRUE,
    full.names = TRUE
  )
  readr::read_lines(proj_codes_file) |>
    tibble::as_tibble() |>
    dplyr::filter(stringr::str_detect(value, "^[a-z]{3}:")) |>
    tidyr::separate(value, c("proj_cd", "proj_nm"), ": ") |>
    dplyr::mutate(proj_cd = stringr::str_c("en_", proj_cd)) |> 
    saveRDS(here("data", "npp_2018b_codes.rds"))
}

read_npp_codes("data-raw")
