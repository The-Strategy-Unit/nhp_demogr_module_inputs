# README ----
# The demographic module requires an estimate of population change specific to
# a provider. Local authority district is the lowest geography that ONS
# projections are produced for so catchments are built on LAD. This is different
# from the input app where catchments are built using LSOAs (to mirror PHE
# methodology) - the input app only requires catchments for historic and current
# time periods - not future time periods. Catchments are built using only
# inpatient activity - assumption that this activity is most broadly
# representative of the population served by a hospital. LAD list needs to match
# HES year otherwise administrative boundary changes will make it difficult to
# tie activity geographies to population geographies.


# packages ----
library("beepr")
library("DBI")
library("dbplyr")
library("dplyr")
library("here")
library("odbc")
library("leaflet")
library("mapview")
library("purrr")
library("readr")
library("readxl")
library("sf")
library("stringr")
library("webshot2")

# db connection ----
con_hes <- dbConnect(
  odbc(),
  Driver = "SQL Server",
  Server = "MLCSU-BI-SQL",
  Database = "HESData",
  Trusted_Connection = "True"
)

tbip <- tbl(con_hes, in_schema("dbo", "tbInpatients"))

# cohort 4 trusts ----
# "RXN_RTX" joint build
# "RH5_RBA" joint build
chrt4_trusts_all <- c(
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
  "RBA",
  "RAS",
  "RCX", # The Queen Elizabeth Hospital, King's Lynn, NHS Foundation Trust
  "RGN", # North West Anglia NHS Foundation Trust
  "RBT", # Mid Cheshire Hospitals NHS Foundation Trust
  "RCF", # Airedale NHS Foundation Trust
  "RDU" # Frimley Health NHS Foundation Trust
)

chrt4_trusts <- c(
  chrt4_trusts_all[!chrt4_trusts_all %in% c("RXN", "RTX", "RH5", "RBA")],
  "RXN_RTX", "RH5_RBA"
)

# read activity ----
ip_activity <- tbip |>
  filter(
    FYEAR == "201819",
    SPELEND == "Y",
    PROCODE3 %in% chrt4_trusts_all
  ) |>
  count(FYEAR, PROCODE3, RESLADST_ONS, LSOA11, SEX) |>
  collect()

beepr::beep(9)
dbDisconnect(con_hes)

# group providers in joint builds
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

# read LAD list ----
# LAD lookup
lad_dec18 <- read_csv(
  here(
    "data-raw",
    "Local_Authority_Districts_December_2018_UK_Names_and_Codes.csv"
  )
)

lad_dec18 <- lad_dec18 |>
  rename_with(tolower, everything()) |>
  filter(str_detect(lad18cd, "^E")) |>
  select(-lad18nmw)

# build catchments ----
catchments <- ip_activity |>
  left_join(lad_dec18, join_by("resladst_ons" == "lad18cd"))

# to limit the number of LADs associated with a single provider (i.e. to avoid
# a long tail) we only include LADs with a 5% or greater share of activity. We
# then redistribute the retained activity to ensure the share across LADs sums
# to 1
catchments <- catchments |>
  group_by(procode) |>
  filter(p >= .05) |>
  mutate(p_adj = n / sum(n)) |>
  ungroup() |>
  select(-n, -p)

# create maps ----
# create maps as a visual check on catchments
# LAD boundaries
lad_bdy <- st_read(
  here(
    "data-raw",
    "Local_Authority_Districts_December_2018_UK_BUC.geojson"
  )
)

lad_bdy <- lad_bdy |>
  filter(str_detect(lad18cd, "^E"))

# leaflet package expects latitude and longitude specified using WGS84
# (EPSG:4326)
# ONS boundaries specified using BNG (EPSG:27700)
lad_bdy <- st_transform(lad_bdy, crs = "EPSG:4326")

# opacity
na_op <- function(x) {
  ifelse(is.na(x), 0, .6)
}

# create palette
# scales::show_col(scales::viridis_pal()(5))
pal <- colorBin(
  "viridis",
  domain = c(0, 1),
  bins = seq(0, 1, .2),
  na.color = NA,
  reverse = TRUE
)

# create maps
catch_map <- function(trust) {

  catch_dat <- lad_bdy |>
    left_join(
      catchments |>
        select(-lad18nm) |>
        filter(procode == {{ trust }}),
      join_by("lad18cd" == "resladst_ons")
    )

  # zoom to relevant geography
  bbox <- st_bbox(
    catch_dat |>
      filter(!is.na(p_adj))
  ) |>
    as.vector()

  # draw map
  m <- leaflet()  |>
    addProviderTiles("CartoDB.Voyager")  |>
    fitBounds(bbox[1], bbox[2], bbox[3], bbox[4]) |>
    addPolygons(
      data = catch_dat,
      weight = .4,
      opacity = 1,
      color = "#440154",
      fillColor = ~ pal(p_adj), fillOpacity = ~ na_op(p_adj),
      label = ~ lad18nm
    )  |>
    addLegend(
      "bottomright",
      title = "activity share",
      pal = pal,
      values = catch_dat$p_adj
    )
}

catch_maps_ls <- map(chrt4_trusts, catch_map)

# bug - mapshot fails to save leaflet map as PNG file (no problem creating
# HTML file)
# workaround uses webshot2
# https://github.com/r-spatial/mapview/issues/419
# mapshot(catch_maps_ls[[1]], file = "test_map.png", debug = TRUE)

# save maps
print_maps <- function(trust, map) {

  html_nm <- paste0("catchment_map_", trust, ".html")
  png_nm  <- str_replace(html_nm, ".html", ".png")

  mapshot(map, url = here("figures", html_nm), debug = TRUE)
  webshot2::webshot(url = here("figures", html_nm), file = here("figures", png_nm))
}

map2(chrt4_trusts, catch_maps_ls, print_maps)

# save ----
write_rds(
  list(chrt4_trusts_all = chrt4_trusts, chrt4_trusts = chrt4_trusts),
  here("data", "cohort4_trust_codes.rds")
)

write_rds(catchments, here("data", "cohort4_trust_catchments_201819.rds"))