# Create weighted catchments to pass to NHP model
# NHP model - demographic module (942)
# author Paul Seamer


# README ----
# Use provider catchments and population data to create weighted catchment populations.
# Weighted populations series are passed as input to NHP model.


# in this script ----
# 1 read files
# 2 create weighted populations
# 3 save


# packages ----
library("tidyverse")
library("here")




# 1 read files ----
nhp_snpp_2018b <- readRDS(here("data","nhp_snpp_2018b.rds"))
catchments     <- readRDS(here("data", "nhp_lad_catchments.rds"))
cohort4        <- readRDS(here("data", "nhp_cohort4_trusts.rds"))
cohort4_trusts_all <- cohort4[["cohort4_trusts_all"]]
cohort4_trusts     <- cohort4[["cohort4_trusts"]]


# 2 weighted pop ----
w_pop <- function(trust, pop_dat) {
  
  catch <- catchments |> 
    filter(procode == {{ trust }})
  
  lad <- catch |>
    pull(resladst_ons)
  
  w_pop <- map(nhp_snpp_2018b,
             ~ . |> 
               filter(area_code %in% lad) |>
               left_join(catch, by = c("area_code" = "resladst_ons")) |>
               group_by(procode, id, sex, age, year, base) |>
               summarise(pop = sum(pop * p_adj)) |> 
               ungroup())
}

# list of trusts; nested list variant projections 
ls_trusts_vars <- map(cohort4_trusts, w_pop)
# list of trusts; all variants in single df
ls_trusts <- lapply(ls_trusts_vars, bind_rows)
# all trusts/variants single df
trusts_dat <- bind_rows(ls_trusts) |> 
  select(procode, base, id, sex, age, year, pop)




# save ----
saveRDS(trusts_dat, here("data", "nhp_cohort4_trusts_demogr_dat.rds"))

