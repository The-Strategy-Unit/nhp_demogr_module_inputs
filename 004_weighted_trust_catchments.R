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
nhp_snpp_2018b       <- readRDS(here("data","nhp_snpp_2018b.rds"))
nhp_snpp_2018b_cvars <- readRDS(here("data","nhp_snpp_2018b_custom_vars.rds"))
catchments           <- readRDS(here("data", "nhp_lad_catchments.rds"))
cohort4              <- readRDS(here("data", "nhp_cohort4_trusts.rds"))
cohort4_trusts_all   <- cohort4[["cohort4_trusts_all"]]
cohort4_trusts       <- cohort4[["cohort4_trusts"]]




# 2 weighted pop ----
w_pop <- function(trust, popdat) {
  
  catch <- catchments |> 
    filter(procode == {{ trust }})
  
  lad <- catch |>
    pull(resladst_ons)
  
  w_pop <- map(popdat,
             ~ . |> 
               filter(area_code %in% lad) |>
               left_join(catch, by = c("area_code" = "resladst_ons")) |>
               group_by(procode, id, sex, age, year, base) |>
               summarise(pop = sum(pop * p_adj)) |> 
               ungroup())
}

# standard snpp variants (x4)
# list of trusts; nested list variant projections 
ls_trusts_vars <- map(cohort4_trusts, ~ w_pop(.x, popdat = nhp_snpp_2018b))
# list of trusts; all variants in single df
ls_trusts <- lapply(ls_trusts_vars, bind_rows)
# all trusts/variants single df
std_trusts_dat <- bind_rows(ls_trusts) |> 
  select(procode, base, id, sex, age, year, pop)

# custom snpp variants (x17)
ls_trusts_vars <- map(cohort4_trusts, ~ w_pop(.x, popdat = nhp_snpp_2018b_cvars))
# list of trusts; all variants in single df
ls_trusts <- lapply(ls_trusts_vars, bind_rows)
# all trusts/variants single df
custom_trusts_dat <- bind_rows(ls_trusts) |> 
  select(procode, base, id, sex, age, year, pop)

# remove duplicate custom variants that also appear in standard variants
# remove - principal (en_ppp), low migration (en_ppl) and high migration (en_pph)
custom_trusts_dat <- custom_trusts_dat |> 
  filter(!id %in% c("en_ppp", "en_ppl", "en_pph")) |> 
  left_join(proj_codes, by = c("id" = "proj_code")) |> 
  select(-id) |> 
  rename(id = proj_name)

# compile single df with x4 standard variants + 15 non-duplicate custom variants
# 4 standard variants + 15 custom variants + principal = 20
trusts_dat <- bind_rows(
  std_trusts_dat |> 
    mutate(source = "standard snpp variant"),
  custom_trusts_dat |> 
    mutate(source = "custom snpp variant")
  ) 




# save ----
saveRDS(trusts_dat, here("data", "nhp_cohort4_trusts_demogr_dat.rds"))

