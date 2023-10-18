# README ----
# Use provider catchments and population data to create weighted catchment
# populations


# packages ----
library("dplyr")
library("here")
library("purrr")
library("readr")
library("tidyr")

# read files ----
snpp_2018b <- read_rds(here("data", "snpp_2018b_syoa_100.rds"))
snpp_2018b_cv <- read_rds(here("data", "snpp_2018b_custom_vars.rds"))
snpp_2018b_bths <- read_rds(here("data", "snpp_2018b_births.rds"))
snpp_2018b_cv_bths <- read_rds(here("data", "snpp_2018b_custom_vars_births.rds")) # nolint: line_length_linter.
catchments <- read_rds(here("data", "cohort4_trust_catchments_201819.rds"))

chrt4 <- read_rds(here("data", "cohort4_trust_codes.rds"))
chrt4_trusts_all <- chrt4[["chrt4_trusts_all"]]
chrt4_trusts <- chrt4[["chrt4_trusts"]]

# weighted pop fn ----
w_pop <- function(trust, dat, var_nm) {

  catch <- catchments |>
    filter(procode == {{ trust }})

  lad <- catch |>
    pull(resladst_ons)

  # helper for selecting grouping variables
  if (deparse(substitute(var_nm)) == "pop") {
    group_vars <- c("procode", "sex", "age", "year")
  } else if (deparse(substitute(var_nm)) == "bths") {
    group_vars <- c("procode", "age", "year")
  } else {
    stop("Error: problem with var_nm argument")
  }

  w_pop <- dat |>
    mutate(data = map(data, \(x) x |>
        filter(area_code %in% lad) |>
        left_join(catch, join_by("area_code" == "resladst_ons"))  |>
        group_by(across(all_of(group_vars))) |>
        summarise(pop = sum({{ var_nm }} * p_adj)) |>
        ungroup()
    )) |>
    unnest(c(data)) |>
    group_by(procode, across(ends_with("id"))) |>
    nest(.key = "data") |>
    ungroup() |>
    select(-starts_with("id"))
}

# populations ----
# standard snpp variants (x4)
ls_trusts_std_vars <- chrt4_trusts |>
  map(\(x) w_pop(trust = x, dat = snpp_2018b, var_nm = pop))

# custom snpp variants (x17)
ls_trusts_cus_vars <- chrt4_trusts |>
  map(\(x) w_pop(trust = x, dat = snpp_2018b_cv, var_nm = pop))

# remove custom variants that are duplicated in standard variants
# remove - principal (npp_ppp), low migration (npp_ppl) and high migration
# (npp_pph)
ls_trusts_cus_vars <- map(
  ls_trusts_cus_vars, \(x) x |>
    filter(!ons_id %in% c("npp_ppp", "npp_ppl", "npp_pph"))
)

# compile single list with x4 standard variants + 15 non-duplicate custom
# variants. 4 standard variants + 15 custom variants + principal = 20
chrt4_wt_catchments <- map2(
  ls_trusts_std_vars, ls_trusts_cus_vars, \(x, y) bind_rows(x, y)
)

# births ----
# standard snpp variants (x4)
ls_trusts_std_vars_bths <- chrt4_trusts |>
  map(\(x) w_pop(trust = x, dat = snpp_2018b_bths, var_nm = bths))

# custom snpp variants (x17)
ls_trusts_cus_vars_bths <- chrt4_trusts |>
  map(\(x) w_pop(trust = x, dat = snpp_2018b_cv_bths, var_nm = bths))

# remove custom variants that are duplicated in standard variants
# remove - principal (npp_ppp), low migration (npp_ppl) and high migration
# (npp_pph)
ls_trusts_cus_vars_bths <- map(
  ls_trusts_cus_vars_bths, \(x) x |>
    filter(!ons_id %in% c("npp_ppp", "npp_ppl", "npp_pph"))
)

# compile single list with x4 standard variants + 15 non-duplicate custom
# variants. 4 standard variants + 15 custom variants + principal = 20
chrt4_wt_catchments_bths <- map2(
  ls_trusts_std_vars_bths, ls_trusts_cus_vars_bths, \(x, y) bind_rows(x, y)
)

# save ----
# populations
write_rds(
  chrt4_wt_catchments,
  here("data", "cohort4_trust_wt_catchment_pops.rds")
)

write_csv(
  bind_rows(chrt4_wt_catchments) |>
    unnest(data),
  here("data", "cohort4_trust_wt_catchment_pops.csv")
)

# births
write_rds(
  chrt4_wt_catchments_bths,
  here("data", "cohort4_trust_wt_catchment_births.rds")
)

write_csv(
  bind_rows(chrt4_wt_catchments_bths) |>
    unnest(data),
  here("data", "cohort4_trust_wt_catchment_births.csv")
)