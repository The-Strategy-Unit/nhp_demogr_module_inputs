
# README ----
# Use provider catchments and population data to create weighted catchment populations.
# Weighted catchment populations are passed as inputs to the model.


# packages ----
{
  library("dplyr")
  library("here")
  library("purrr")
  library("tidyr")
}


# read files ----
snpp_2018b             <- readRDS(here("data","snpp_2018b_syoa_100.rds"))
snpp_2018b_custom_vars <- readRDS(here("data","snpp_2018b_custom_vars.rds"))
proj_codes             <- readRDS(here("data","npp_2018b_codes.rds"))
catchments             <- readRDS(here("data", "cohort4_trust_catchments_201819.rds"))

chrt4            <- readRDS(here("data", "cohort4_trust_codes.rds"))
chrt4_trusts_all <- chrt4[["chrt4_trusts_all"]]
chrt4_trusts     <- chrt4[["chrt4_trusts"]]


# weighted pop ----
w_pop <- function(trust, popdat) {
  
  catch <- catchments |> 
    filter(procode == {{ trust }})
  
  lad <- catch |>
    pull(resladst_ons)
  
  w_pop <- popdat |> 
    mutate(data = map(data, \(x) x |>
                        filter(area_code %in% lad) |> 
                        left_join(catch, by = c("area_code" = "resladst_ons"))  |>
                        group_by(procode, sex, age, year)  |>
                        summarise(pop = sum(pop * p_adj)) |> 
                        ungroup())
           ) |> 
    unnest(data) |> 
    group_by(procode, across(ends_with("id"))) |> 
    nest(.key = "data")
}

# standard snpp variants (x4)
ls_trusts_std_vars <- chrt4_trusts |> map(\(x) w_pop(trust = x, popdat = snpp_2018b))
ls_trusts_std_vars <- map(ls_trusts_std_vars, \(x) x |> rename(id = map_id))

# custom snpp variants (x17)
ls_trusts_cus_vars <- chrt4_trusts |> map(\(x) w_pop(trust = x, popdat = snpp_2018b_custom_vars))

# remove custom variants that are duplicated in standard variants
# remove - principal (en_ppp), low migration (en_ppl) and high migration (en_pph)
ls_trusts_cus_vars <- map(ls_trusts_cus_vars, \(x) x |> 
                            filter(!id %in% c("en_ppp", "en_ppl", "en_pph")))

# compile single list with x4 standard variants + 15 non-duplicate custom variants
# 4 standard variants + 15 custom variants + principal = 20
chrt4_wt_catchments <- map2(ls_trusts_std_vars, ls_trusts_cus_vars, \(x, y) bind_rows(x, y))
  

# save ----
saveRDS(chrt4_wt_catchments, here("data", "cohort4_trust_wt_catchment_pops.rds"))

