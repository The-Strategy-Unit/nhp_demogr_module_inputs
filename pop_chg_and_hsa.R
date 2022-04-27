# Compile ONS population projections and life tables
# NHP model - demographic module (942)
# author Paul Seamer


# README ----
# The demographic module requires information on 
# (a) projected population change, and
# (b) projected cohort expectations of life (for the health status adjustment)
# Sub-national population projections SNPP are cut at age 90+. We use national
# projections to estimate the age distribution for age 90-100+.
# x17 variant projections are published as part of the 2018b NPP
# x4 variant projections are published as part of the 2018b SNPP
# High and low LE variants of expectation of life are published as part of the 2018b NPP.
# All datasets used are for England.


# in this script ----
# 1 read SNPPP 2018b
# 2 read NPP 2018b
# 3 read 2018 and 2020 life tables
# 4 spread SNPP age 90+ to 90-100+
# 5 tests
# 6 save


# packages ----
library("tidyverse")
library("here")
library("xml2")
library("readxl")
library("testthat")

# dplyr::group_split() returns a list of tibbles - functionality to return a named
# list was considered anti-pattern - this is a work around to add names. 
# https://github.com/tidyverse/dplyr/issues/4223
named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::inject(paste(!!!group_keys(grouped), sep = " / "))
  
  grouped %>% 
    group_split() %>% 
    rlang::set_names(names)
}




# 1 snpp 2018b ----
proj_v <- "snpp_2018b"
proj_vars <- list.files(here("_raw_data"), proj_v, full.names = TRUE)

# clean df
clean_df <- function(df) {
  
  dat <- df |> 
    rename_all(tolower) |> 
    pivot_longer(cols = `2018`:`2043`, names_to = "year", values_to = "pop") |> 
    select(-component) |> 
    # filter out regions & counties
    filter(str_detect(area_code, "^(E12)|^(E11)|^(E10)", negate = TRUE)) |> 
    # filter out all ages count
    filter(age_group != "All ages")
}

# read variants
read_vars <- function(proj_vars) {
  
  out_list <- vector("list", length = length(proj_vars))
  
  for (i in seq_along(proj_vars)) {
  
  base <- "2018b"
  id <- str_extract(proj_vars[i], "(?<=2018b_).*")
  files <- list.files(proj_vars[i], "^(2018 SNPP).*(females|males).*(.csv$)", full.names = TRUE)
  
  l <- map(files, ~ read_csv(.x))
  
  dat <- bind_rows(l, .id = NULL) |> 
    clean_df() |> 
    mutate(base = base, id = id)

  out_list[[i]] <- dat
  names(out_list)[i] <- id
  
  }
  
  return(out_list)
  
}

snpp_2018b_dat <- read_vars(proj_vars)




# 2 npp 2018b ----
proj_v <- "npp_2018b"
proj_vars <- list.files(here("_raw_data", proj_v), "(.xml)$", full.names = TRUE)

proj_codes <- tribble(
  ~proj_code, ~proj_name,
  "ppp", "Principal projection",
  "hpp", "High fertility",
  "lpp", "Low fertility",
  "php", "High life expectancy",
  "plp", "Low life expectancy",
  "pph", "High migration",
  "ppl", "Low migration",
  "hhh", "High population",
  "lll", "Low population",
  "lhl", "Old age structure",
  "hlh", "Young age structure",
  "ppz", "Zero net migration (natural change only)",
  "pnp", "No mortality improvement",
  "cnp", "Constant fertility, no mortality improvement",
  "cpp", "Constant fertility",
  "rpp", "Replacement fertility",
  "ppr", "50% Future EU migration (Not National Statistics)",
  "ppq", "0% Future EU migration (Not National Statistics)")

proj_codes <- proj_codes |> mutate(proj_code = str_c("en_", proj_code))

# clean df
clean_df <- function(df) {
  
  dat <- df |> 
    rename_all(tolower) |> 
    pivot_longer(cols = `2018`:`2118`, names_to = "year", values_to = "pop") |> 
    mutate(age = str_trim(age), area = "England", pop = as.double(pop)) |> 
    mutate(sex = ifelse(sex == "1", "males", "females"))
}

# read variants
read_vars <- function(proj_vars) {
  
  out_list <- vector("list", length = length(proj_vars))
  
  for (i in seq_along(proj_vars)) {
    
    base <- "2018b"
    id <- str_extract(proj_vars[i], "(?<=npp_2018b/).*(?=_opendata)")
    file <- proj_vars[i]
    
    xml_doc <- read_xml(file)
    xml_l <- as_list(xml_doc)
    tbl <- xml_l$Workbook[[8]]$Table
    first_row <- xml_l$Workbook[[8]]$Table$Row
    colnms <- unlist(first_row) |> unname()
    dat <- as_tibble(t(matrix(unlist(tbl[-c(1, 4)]), ncol = 214)), .name_repair = ~ colnms)
    
    dat <- dat |> 
      clean_df() |> 
      mutate(base = base, id = id)
    
    out_list[[i]] <- dat
    names(out_list)[i] <- id
    
  }
  
  return(out_list)
  
}

npp_2018b_dat <- read_vars(proj_vars)

npp_2018b_dat <- map(npp_2018b_dat, ~ .x |> left_join(proj_codes, by = c("id" = "proj_code")))




# 3 life tables ----
# 2020b
read_lt20 <- function(wb, sheet) {
  
  base <- "2020b"
  dat <- read_xlsx(here("_raw_data", wb), sheet = sheet, skip = 4)
  
  dat <- dat |> 
    mutate(base = base) |> 
    pivot_longer(cols = `1981`:`2070`, names_to = "year", values_to = "ex")
}

ex20m <- read_lt20(wb = "engppp20ex.xlsx", sheet = "males cohort ex")
ex20f <- read_lt20(wb = "engppp20ex.xlsx", sheet = "females cohort ex")

ex_2020b_dat <- ex20m |> 
  mutate(sex = "m") |> 
  bind_rows(ex20f |> mutate(sex = "f"))

# 2018b
read_lt18 <- function(wb, sheet) {
  
  base <- "2018b"
  dat <- read_xls(here("_raw_data", wb), sheet = sheet, skip = 9)
  
  dat <- dat |> 
    filter(!row_number() == 1L) |> 
    rename_with(.cols = 1, ~ "age") |> 
    mutate(base = base) |> 
    pivot_longer(cols = `1981`:`2068`, names_to = "year", values_to = "ex")
}

ex18m <- read_lt18(wb = "engppp18ex.xls", sheet = "Males cohort ex")
ex18f <- read_lt18(wb = "engppp18ex.xls", sheet = "Females cohort ex")

ex_2018b_dat <- ex18m |> 
  mutate(sex = "m") |> 
  bind_rows(ex18f |> mutate(sex = "f"))




# 4 spread age 90+ ----
# Which NPP variant should we take the 90+ distribution from?
# Ideally, there exists a 1:1 mapping of SNPP variants to NPP variants.
# If this is not the case we need to create one.

# 2018b SNPP
# names(snpp_2018b_dat)

# 2018b NPP
# proj_codes

# create mapping
var_map <- tribble(
  ~snpp_id, ~npp_id,
  "principal_proj", "en_ppp",
  "var_proj_10_year_migration", "en_ppp",
  "var_proj_alt_internal_migration", "en_ppp",
  "var_proj_high_intl_migration", "en_pph",
  "var_proj_low_intl_migration", "en_ppl")

# extract 90+ distribution from NPP
extract_90 <- function(x) {
  
  x |> 
    filter(str_detect(age, "^1[0-1][0-9]|^9[0-9]")) |> 
    mutate(age = case_when(age %in% c(
      "100", "101", "102", "103", "104",
      "105 - 109", "110 and over") ~ "100",
      TRUE ~ as.character(age))) |>
    group_by(across(-c(pop))) |> 
    summarise(pop = sum(pop)) |> 
    group_by(across(-c(age, pop))) |> 
    mutate(pop_pct = pop / sum(pop)) |> 
    ungroup() |> 
    mutate(age = as.integer(age)) |> 
    select(-pop, -area, -base) |> 
    arrange(year, sex, age)
    
  }

dist_90 <- map(npp_2018b_dat, ~ extract_90(.x))

# only keep npp variants that have been mapped to snpp variants
dist_90 <- dist_90[unique(var_map$npp_id)]

# How different is the distribution across variants?
# dist_90_df <- map_df(dist_90, ~ bind_rows(.x))
# 
# ggplot(dist_90_df |> filter(sex == "males")) +
#   geom_line(aes(x = year, y = pop_pct, group = age, color = age)) +
#   facet_wrap(vars(id))
# 
# dist_90_df |>
#   mutate(proj_name = factor(proj_name, levels = proj_codes$proj_name)) |>
#   filter(sex == "females", age == "100", year == "2030") |>
#   arrange(proj_name)

# spread snpp 90+
spread_90 <- function(x) {
  
  x |> 
  filter(age_group == "90 and over") |> 
  left_join(var_map, by = c("id" = "snpp_id")) |> 
  mutate(age = 90L)  %>% 
  select(-age_group) |> 
  group_by(area_code, area_name, sex, year) |> 
  complete(age = 90:100) |> 
  fill(base, id, npp_id)
  
}
  
new_90 <- map(snpp_2018b_dat, ~ spread_90(.x))

# join npp distribution to snpp to obtain updated 90+ distribution
updated_90 <- list(bind_rows(new_90), bind_rows(dist_90)) %>%
  reduce(
    function(x, y)
      left_join(x, y, by =  c("npp_id" = "id", "year", "sex", "age")) |> 
      group_by(id, area_code, area_name, sex, year) |> 
      mutate(pop = pop_pct * sum(pop, na.rm = TRUE)) |> 
      ungroup()
    ) |> 
  select(-npp_id, -proj_name, -pop_pct) |> 
  named_group_split(id)

# extract snpp for 0-89 
snpp_0to89 <- map(snpp_2018b_dat, ~ .x |> 
                    filter(age_group != "90 and over") |> 
                    mutate(age_group = as.integer(age_group)) |> 
                    rename(age = age_group))

# append updated 90+ distribution
nhp_snpp_2018b <- map2(snpp_0to89, updated_90, bind_rows)



# 5 tests ----
# check new 90+ total equals old 90+ total
source(here("tests", "testthat", "test-nhp-custom-snpp.R"))




# 6 save ----
# new
saveRDS(nhp_snpp_2018b, here("data","nhp_snpp_2018b.rds"))
# originals
saveRDS(npp_2018b_dat, here("data","npp_2018b_dat.rds"))
saveRDS(snpp_2018b_dat, here("data","snpp_2018b_dat.rds"))
saveRDS(ex_2020b_dat, here("data","ex_2020b_dat.rds"))
saveRDS(ex_2018b_dat, here("data","ex_2018b_dat.rds"))

