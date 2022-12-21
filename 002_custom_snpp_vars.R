# Create custom sub-national projection variants
# NHP model - demographic module (942)
# author Paul Seamer


# README ----
# x4 variant projections are published as part of the 2018b SNPP
# x17 variant projections are published as part of the 2018b NPP
# this script creates a set of x17 custom sub-national projection variants based
# on applying the % difference between NPP variants (by age/sex/year) and NPP
# principal to the SNPP baseline


# in this script ----
# 1 read
# 2 create custom variants
# 3 tests
# 4 finalise output
# 5 save


# packages ----
library("tidyverse")
library("here")
library("testthat")

# pull differences
var_diff_mx <- function(npp_ppp_df, npp_var_proj_df) {
  npp_var_proj_df <- npp_var_proj_df |>
    filter(year %in% as.character(2018:2043)) |> # snpp ends at 2043
    mutate(age = case_when(
      age %in% c(as.character(100:104), "105 - 109", "110 and over")
      ~ "100", TRUE ~ age
    )) |>
    group_by(sex, age, year) |> # re-sum by age
    summarise(var_pop = sum(pop)) |>
    ungroup()
  
  diff_mx <- npp_ppp_df |>
    left_join(npp_var_proj_df, by = c("year", "sex", "age")) |>
    mutate(mx = var_pop / pop, age = as.integer(age)) |>
    select(year, sex, age, mx)
}

# apply differences
app_mx <- function(snpp_ppp_df, mx) {
  snpp_ppp_df |>
    left_join(mx, by = c("year", "sex", "age")) |>
    mutate(new_pop = mx * pop, .before = pop) |>
    select(-pop, -mx) |>
    rename(pop = new_pop)
}




# 1 read ----
nhp_snpp_2018b <- readRDS(here("data", "nhp_snpp_2018b.rds"))
npp_2018b_dat <- readRDS(here("data", "npp_2018b_dat.rds"))
# variant codes
proj_codes <- readRDS(here("data", "proj_var_codes.rds"))




# 2 create custom variants ----
# npp principal
npp_ppp_df <- npp_2018b_dat$en_ppp |>
  filter(year %in% as.character(2018:2043)) |> # snpp ends at 2043
  mutate(age = case_when(
    age %in% c(as.character(100:104), "105 - 109", "110 and over")
    ~ "100", TRUE ~ age
  )) |>
  group_by(sex, age, year) |> # re-sum by age
  summarise(pop = sum(pop)) |>
  ungroup()

# remove npp principal projection
npp_2018b_vars <- npp_2018b_dat |> list_modify("en_ppp" = NULL)

# pull differences
ls_mx <- map(npp_2018b_vars, ~ var_diff_mx(npp_ppp_df, .x))
ls_mx <- imap(ls_mx, ~ .x |> mutate(id = str_replace(.y, "en_", "npp_")))

# apply differences
ls_snpp_custom_vars <- map(ls_mx, ~ app_mx(nhp_snpp_2018b$principal_proj |> select(-id), .))




# 3 tests ----
# a) test consistency of custom variant order v. NPP order
# pick a year to test
test_yr <- "2035"

# assemble ranking for custom sub-national variants
ls_vars_test_yr <- map(ls_snpp_custom_vars, ~ .x |>
                         filter(year == test_yr) |>
                         group_by(area_name) |>
                         summarise(n = sum(pop)))

snpp_ppp_test_yr <- nhp_snpp_2018b$principal_proj |>
  filter(year == test_yr) |>
  group_by(area_name) |>
  summarise(n = sum(pop))

ls_vars_test_yr <- imap(ls_vars_test_yr, ~ .x |> mutate(id = .y))

df_vars_test_yr <- bind_rows(ls_vars_test_yr) |>
  bind_rows(
    snpp_ppp_test_yr |>
      mutate(id = "en_ppp")
  )

rnk_vars_test_yr <- df_vars_test_yr |>
  group_by(area_name) |>
  arrange(area_name, n) |>
  mutate(vars_rnk = 1:n()) |>
  group_by(id, vars_rnk) |>
  summarise(n = n()) |>
  ungroup() |>
  arrange(vars_rnk) |>
  left_join(proj_codes, by = c("id" = "proj_code"))

# assemble ranking for NPP variants (benchmark)
ls_npp_test_yr <- map(npp_2018b_dat, ~ .x |>
                        filter(year == test_yr) |>
                        summarise(n = sum(pop)))

ls_npp_test_yr <- imap(ls_npp_test_yr, ~ .x |> mutate(id = .y))

df_npp_test_yr <- bind_rows(ls_npp_test_yr)

rnk_npp_test_yr <- df_npp_test_yr |>
  arrange(n) |>
  mutate(npp_rnk = 1:n()) |>
  left_join(proj_codes, by = c("id" = "proj_code"))

rnk_diffs <- rnk_vars_test_yr |>
  left_join(
    rnk_npp_test_yr |>
      select(-n, -proj_name),
    by = "id"
  ) |>
  mutate(diff_rnk = vars_rnk - npp_rnk) |>
  arrange(id, diff_rnk)

# b) test difference between variants that are in both SNPP and NPP, by LA
ls_npp_area_test_yr <- map(nhp_snpp_2018b, ~ .x |>
                             filter(year == test_yr) |>
                             group_by(area_name) |>
                             summarise(n = sum(pop)))

ls_npp_area_test_yr <- imap(ls_npp_area_test_yr, ~ .x |> mutate(id = .y))

df_npp_area_test_yr <- bind_rows(ls_npp_area_test_yr) |>
  filter(id %in% c(
    "principal_proj",
    "var_proj_low_intl_migration",
    "var_proj_high_intl_migration")
  )

ls_common_vars_test_yr <- ls_vars_test_yr[c("en_pph", "en_ppl")]

df_common_vars_test_yr <- bind_rows(ls_common_vars_test_yr)

cf_vars <- bind_rows(df_npp_area_test_yr, df_common_vars_test_yr) |>
  pivot_wider(names_from = "id", values_from = "n") |>
  mutate(
    p_high = (en_pph / var_proj_high_intl_migration - 1L) * 100,
    p_low = (en_ppl / var_proj_low_intl_migration - 1L) * 100) |>
  select(area_name, p_high:p_low)

# plot differences
# ggplot(data = cf_vars, aes(x = reorder(area_name, p_high), y = p_high, group = 1L)) +
#   geom_point(color = "#2c2d54") +
#   geom_point(aes(x = area_name, y = p_low, group = 1L), color = "#87bcbd") +
#   scale_x_discrete(name = NULL) +
#   scale_y_continuous(name = NULL) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(.8)))

source(here("tests", "testthat", "test_002_custom_snpp_vars.R"))




# 4 finalise outputs ----
names(ls_snpp_custom_vars) <- str_replace(names(ls_snpp_custom_vars), "en_", "npp_")

nms_lkup <- tribble(
  ~oldnm, ~newnm,
  "principal_proj", "snpp_ppp",
  "var_proj_low_intl_migration", "snpp_ppl",
  "var_proj_high_intl_migration", "snpp_pph",
  "var_proj_10_year_migration", "snpp_10m",
  "var_proj_alt_internal_migration", "snpp_aim"
  )

names(nhp_snpp_2018b) <- nms_lkup$newnm[match(names(nhp_snpp_2018b), nms_lkup$oldnm)]

nhp_snpp_2018b <- imap(nhp_snpp_2018b, ~ .x |>
       left_join(nms_lkup, by = c("id" = "oldnm")) |> 
       select(-id) |> 
       rename(id = newnm))

# remove low/high migration variants from custom NPP set (these are in SNPP set)
# ls_snpp_custom_vars$npp_ppl <- NULL
# ls_snpp_custom_vars$npp_pph <- NULL

ls_snpp_custom_vars <- c(nhp_snpp_2018b, ls_snpp_custom_vars)




# 5 save ----
saveRDS(ls_snpp_custom_vars, here("data", "nhp_snpp_2018b_custom_vars.rds"))
