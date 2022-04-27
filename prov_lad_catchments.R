# Build population catchments for providers
# NHP model - demographic module (942)
# author Paul Seamer


# JUST NEED TO ADD NOTE ON PROVIDER UPDATE


# README ----
# The demographic module requires an estimate of population change specific to
# a provider. Local authority district is the lowest geography that ONS projections
# are produced for so catchments are built on LAD. This is different from the input
# report where catchments are built using LSOAs (to mirror PHE methodology) - the
# input report only requires catchments for historic and current time periods - not
# future time periods.
# PROVIDER UPDATE .....
# Catchments are built using only inpatient activity - assumption that this activity
# is most broadly representative of the population served by a hospital.


# in this script ----
# 1 read inpatient activity
# 2 read LAD list
# 3 build catchments
# 4 test
# 5 save


# packages ----
library("tidyverse")
library("here")
library("readxl")
library("DBI")
library("odbc")
library("dbplyr")
library("sf")
library("leaflet")
library("mapview")

con_hes <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "MLCSU-BI-SQL",
                     Database = "HESData",
                     Trusted_Connection = "True"
)

tbip <- tbl(con_hes, in_schema("dbo", "tbInpatients"))

update_providers <- readRDS(here("data", "update_providers_20220427.RDS"))


# 1 read activity ----
ip_activity <- tbip |> 
  filter(FYEAR == "201819") |> 
  filter(SPELEND == "Y") |> 
  count(FYEAR, PROCODE3, RESLADST_ONS, LSOA11, SEX) |> 
  collect()

beepr::beep(9)

# clean
ip_activity <- ip_activity |>
  ungroup() |> 
  rename_with(tolower, everything()) |> 
  select(-fyear) |> 
  filter(str_detect(lsoa11, "^E"), !is.na(lsoa11)) |> 
  # update providers
  left_join(update_providers, by = c("procode3" = "old_code")) |> 
  mutate(procode3 = case_when(is.na(new_code) ~ procode3, TRUE ~ new_code)) |> 
  group_by(procode3, resladst_ons) |> 
  summarise(n = sum(n)) |> 
  mutate(p = n / sum(n)) |> 
  ungroup()




# 2 read LAD list ----
# LAD lookup
lad_d21 <- read_xlsx(here("_raw_data", "LAD_DEC_2021_UK_NC.xlsx"), sheet = "LAD_DEC_2021_UK_NC")

# clean
lad_d21 <- lad_d21 |> 
  rename_with(tolower, everything()) |> 
  filter(str_detect(lad21cd, "^E")) |> 
  select(-lad21nmw)




# 3 build catchments ----
catchments <- ip_activity |> 
  left_join(lad_d21, by = c("resladst_ons" = "lad21cd"))

# To limit the number of LADs associated with a single provider (i.e. to avoid
# a long tail) we only include LADs with a 5% or greater share of activity. We then
# redistribute the retained activity to ensure the share across LADs sums to 1.
catchments <- catchments |> 
  group_by(procode3) |> 
  filter(p >= .05) |> 
  mutate(p_adj = n / sum(n)) |> 
  ungroup() |> 
  select(-n, -p)




# 4 test ----
# draw a map to check the catchments look correct

# LAD boundaries
lad_bdy <- st_read(here("_raw_data", "Local_Authority_Districts_(December_2021)_GB_BUC.geojson"))
lad_bdy <- lad_bdy |> 
  filter(str_detect(LAD21CD, "^E"))

# select a provider
pro_code <- "RL4"

catch_dat <- lad_bdy |>
  left_join(
    catchments |>
      filter(procode3 == pro_code),
    by = c("LAD21CD" = "resladst_ons"))

# create a palette
# scales::show_col(scales::viridis_pal()(5))
pal <- colorBin("viridis", domain = catch_dat$p_adj, bins = seq(0, 1, .2), na.color = NA, reverse = T)

# opacity fx
na_op <- function(x) {
  ifelse(is.na(x), 0, .6)
}

# zoom to relevant geography
bbox <- st_bbox(
  catch_dat |> 
    filter(!is.na(p_adj))
  ) |> 
  as.vector()

# draw a map
m <- leaflet() %>% 
  addProviderTiles("CartoDB.Voyager") %>% 
  fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) |> 
  addPolygons(data = catch_dat,
              weight = .2,
              opacity = 1,
              color = "#808080",
              fillColor = ~ pal(p_adj), fillOpacity = ~ na_op(p_adj),
              label = ~LAD21NM) %>% 
  addLegend("bottomright", title = "activity share", pal = pal, values = catch_dat$p_adj)

# https://github.com/r-spatial/mapview/issues/419
# m = mapview(breweries)
# mapshot(m, url = paste0(getwd(), "/map.html"))
# mapshot(m, file = paste0(getwd(), "/map.png"))


# 5 save ----
saveRDS(catchments, here("data", "nhp_lad_catchments.rds"))

