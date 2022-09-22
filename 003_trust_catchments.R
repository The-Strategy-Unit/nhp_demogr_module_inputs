# Create trust population catchments
# NHP model - demographic module (942)
# author Paul Seamer


# README ----
# The demographic module requires an estimate of population change specific to
# a provider. Local authority district is the lowest geography that ONS projections
# are produced for so catchments are built on LAD. This is different from the input
# report where catchments are built using LSOAs (to mirror PHE methodology) - the
# input report only requires catchments for historic and current time periods - not
# future time periods.
# Catchments are built using only inpatient activity - assumption that this activity
# is most broadly representative of the population served by a hospital.
# LAD list needs to match HES year otherwise administrative boundary changes will
# make it difficult to tie activity geographies to population geographies.


# in this script ----
# 1 read inpatient activity
# 2 read LAD list
# 3 build catchments
# 4 create maps
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
library("webshot2")


# db connection ----
con_hes <- dbConnect(odbc(),
                     Driver = "SQL Server",
                     Server = "MLCSU-BI-SQL",
                     Database = "HESData",
                     Trusted_Connection = "True"
)

tbip <- tbl(con_hes, in_schema("dbo", "tbInpatients"))


# cohort 4 trusts ----
# "RXN_RTX" joint build
# "RH5_RBA" joint build
cohort4_trusts_all <- c(
  "RXC",
  "RN5",
  "RYJ",
  "RGP",
  "RNQ",
  "RD8",
  "RBZ",
  "RX1",
  "RHW",
  "RA9",
  "RGR",
  "RXN",
  "RTX",
  "RH5",
  "RBA"
)

cohort4_trusts <- c(cohort4_trusts_all[!cohort4_trusts_all %in% c("RXN", "RTX", "RH5", "RBA")], "RXN_RTX", "RH5_RBA")




# 1 read activity ----
ip_activity <- tbip |> 
  filter(FYEAR == "201819") |> 
  filter(SPELEND == "Y") |> 
  filter(PROCODE3 %in% cohort4_trusts_all) |> 
  count(FYEAR, PROCODE3, RESLADST_ONS, LSOA11, SEX) |> 
  collect()

beepr::beep(9)
dbDisconnect(con_hes)

# clean & group providers (joint builds)
ip_activity <- ip_activity |>
  ungroup() |>
  rename_with(tolower, everything()) |>
  select(-fyear) |>
  filter(str_detect(lsoa11, "^E"), !is.na(lsoa11)) |>
  mutate(procode = case_when(
    procode3 %in% c("RXN", "RTX") ~ "RXN_RTX",
    procode3 %in% c("RH5", "RBA") ~ "RH5_RBA",
    TRUE ~ procode3
  )) |>
  group_by(procode, resladst_ons) |>
  summarise(n = sum(n)) |>
  mutate(p = n / sum(n)) |>
  ungroup()




# 2 read LAD list ----
# LAD lookup
lad_dec18 <- read_csv(here("_raw_data", "LAD_DEC_2018_UK_NC.csv"))

# clean
lad_dec18 <- lad_dec18 |> 
  rename_with(tolower, everything()) |> 
  filter(str_detect(lad18cd, "^E")) |> 
  select(-lad18nmw)




# 3 build catchments ----
catchments <- ip_activity |> 
  left_join(lad_dec18, by = c("resladst_ons" = "lad18cd"))

# To limit the number of LADs associated with a single provider (i.e. to avoid
# a long tail) we only include LADs with a 5% or greater share of activity. We then
# redistribute the retained activity to ensure the share across LADs sums to 1.
catchments <- catchments |> 
  group_by(procode) |> 
  filter(p >= .05) |> 
  mutate(p_adj = n / sum(n)) |> 
  ungroup() |> 
  select(-n, -p)




# 4 create maps ----
# create maps to check the catchments look correct
# LAD boundaries
lad_bdy <- st_read(here("_raw_data", "Local_Authority_Districts_(December_2018)_GB_BUC.geojson"))
lad_bdy <- lad_bdy |> 
  filter(str_detect(lad18cd, "^E"))

# opacity
na_op <- function(x) {
  ifelse(is.na(x), 0, .6)
}

# create palette
# scales::show_col(scales::viridis_pal()(5))
pal <- colorBin("viridis", domain = c(0, 1), bins = seq(0, 1, .2), na.color = NA, reverse = T)


# create maps
catch_map <- function(trust) {
  
  catch_dat <- lad_bdy |>
    left_join(
      catchments |> 
        select(-lad18nm) |> 
        filter(procode == {{ trust }}),
      by = c("lad18cd" = "resladst_ons"))
  
  # zoom to relevant geography
  bbox <- st_bbox(
    catch_dat |> 
      filter(!is.na(p_adj))
  ) |> 
    as.vector()
  
  # draw map
  m <- leaflet() %>% 
    addProviderTiles("CartoDB.Voyager") %>% 
    fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) |> 
    addPolygons(data = catch_dat,
                weight = .4,
                opacity = 1,
                color = "#440154",
                fillColor = ~ pal(p_adj), fillOpacity = ~ na_op(p_adj),
                label = ~lad18nm) %>% 
    addLegend("bottomright", title = "activity share", pal = pal, values = catch_dat$p_adj)
}

catch_maps_ls <- map(cohort4_trusts, catch_map)

# bug - mapshot fails to save leaflet map as PNG file (no problem creating HTML file)
# workaround uses webshot2
# https://github.com/r-spatial/mapview/issues/419
# mapshot(m, file = "test_map.png", debug = TRUE)

# save maps out
print_maps <- function(trust, map) {
  
  html_nm <- paste0("catchment_map_", trust, ".html")
  png_nm  <- str_replace(html_nm, ".html", ".png")
  
  mapshot(map, url = here("maps", html_nm), debug = TRUE)
  webshot2::webshot(url = here("maps", html_nm), file = here("maps", png_nm))
}

map2(cohort4_trusts, catch_maps_ls, print_maps)




# 5 save ----
saveRDS(
  list(cohort4_trusts_all = cohort4_trusts_all, cohort4_trusts = cohort4_trusts),
  here("data", "nhp_cohort4_trusts.rds"))
saveRDS(catchments, here("data", "nhp_lad_catchments.rds"))

