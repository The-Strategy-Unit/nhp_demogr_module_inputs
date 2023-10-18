# README ----
# Push copies of files required by NHP model to Posit Connect using pins

# packages ----
library("dplyr")
library("here")
library("pins")
library("readr")

# read datasets ----
c4_trust_wt_catch_pops <- read_csv(
  here("data", "cohort4_trust_wt_catchment_pops.csv")
)
c4_trust_wt_catch_births <- read_csv(
  here("data", "cohort4_trust_wt_catchment_births.csv")
)
le_change <- read_csv(here("data", "life_expectancy_change.csv"))
split_normal_params <- read_csv(here("data", "split_normal_parameters.csv"))
lookup_proj_var <- read_csv(here("data", "lookup_proj_var.csv"))

# push to Connect ----
board <- board_connect()

# write to pins
board |> pin_write(
  title = "NHP demographic data",
  name = "cohort4_trust_wt_catchment_pops",
  x = c4_trust_wt_catch_pops,
  type = "csv"
)
board |> pin_write(
  title = "NHP demographic data",
  name = "cohort4_trust_wt_catchment_births",
  x = c4_trust_wt_catch_births,
  type = "csv"
)
board |> pin_write(
  title = "NHP demographic data",
  name = "life_expectancy_change",
  x = le_change,
  type = "csv"
)
board |> pin_write(
  title = "NHP demographic data",
  name = "split_normal_parameters",
  x = split_normal_params,
  type = "csv"
)
board |> pin_write(
  title = "NHP demographic data",
  name = "lookup_proj_var",
  x = lookup_proj_var,
  type = "csv"
)