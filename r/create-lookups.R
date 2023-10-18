# README ----
# Keep lookups here


# packages ----
library("dplyr")
library("here")
library("readr")
library("stringr")

# lookup from projection variant to life table variant ----
lookup_proj_var <- tribble(
  ~short_nm, ~long_nm, ~ons_id,
  # snpp variants
  "principal_proj", "Principal projection", "principal_proj",
  "10_year_migration", "10 year migration trends", "var_proj_10_year_migration",
  "alt_internal_migration", "5 year internal migration trend", "var_proj_alt_internal_migration", # nolint: line_length_linter.
  "high_intl_migration", "High migration", "var_proj_high_intl_migration",
  "low_intl_migration", "Low migration", "var_proj_low_intl_migration",
  # npp variants
  "principal_proj", "Principal projection", "npp_ppp",
  "high_fertility", "High fertility", "npp_hpp",
  "low_fertility", "Low fertility", "npp_lpp",
  "high_life_expectancy", "High life expectancy", "npp_php",
  "low_life_expectancy", "Low life expectancy", "npp_plp",
  "high_intl_migration", "High migration", "npp_pph",
  "low_intl_migration", "Low migration", "npp_ppl",
  "high_population", "High population", "npp_hhh",
  "low_population", "Low population", "npp_lll",
  "old_age_structure", "Old age structure", "npp_lhl",
  "young_age_structure", "Young age structure", "npp_hlh",
  "zero_net_migration", "Zero net migration (natural change only)", "npp_ppz",
  "no_mortality_improvement", "No mortality improvement", "npp_pnp",
  "const_fert_no_mort_imp", "Constant fertility no mortality improvement", "npp_cnp", # nolint: line_length_linter.
  "const_fertility", "Constant fertility", "npp_cpp",
  "replacement_fertility", "Replacement fertility", "npp_rpp",
  "half_eu_migration", "50% Future EU migration (Not National Statistics)", "npp_ppr", # nolint: line_length_linter.
  "zero_eu_migration", "0% Future EU migration (Not National Statistics)", "npp_ppq" # nolint: line_length_linter.
) |>
  # maps snpp to npp
  mutate(id = case_when(
    ons_id == "principal_proj" ~ "ppp",
    ons_id == "var_proj_10_year_migration" ~ "ppp",
    ons_id == "var_proj_alt_internal_migration" ~ "ppp",
    ons_id == "var_proj_high_intl_migration" ~ "pph",
    ons_id == "var_proj_low_intl_migration" ~ "ppl",
    str_detect(ons_id, "^npp_") ~ str_remove(ons_id, "npp_"),
    TRUE ~ ons_id
  )) |>
  # maps projections to life tables
  mutate(le_var = case_when(
    str_sub(id, 2, 2) == "h" ~ "hle",
    str_sub(id, 2, 2) == "l" ~ "lle",
    TRUE ~ "ppp"
  ))

# save ----
lookup_proj_var |> write_csv(here("data", "lookup_proj_var.csv"))