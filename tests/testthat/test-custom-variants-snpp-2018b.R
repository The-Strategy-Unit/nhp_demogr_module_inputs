
x <- read_rds(here("data", "snpp_2018b_custom_vars.rds"))
y <- read_rds(here("data", "npp_2018b.rds"))
z <- read_rds(here("data", "snpp_2018b_syoa_100.rds"))

# A) test consistency of custom variant total population ranking v. npp rank by LA
# pick a year to test
test_yr <- "2035"

cus_rnk <- custom |> 
  mutate(data = map(data, \(x) x |> 
                      filter(year == test_yr) |> 
                      filter(str_detect(area_code, "^E07", negate = TRUE)) |>
                      group_by(area_code, area_name) |> 
                      summarise(pop = sum(pop)) |> 
                      ungroup()
                    )
  ) |> 
  unnest(data) |> 
  group_by(area_code, area_name) |>
  arrange(pop) |> 
  mutate(cus_rnk = 1:n()) |> 
  ungroup()

npp_rnk <- npp_dat |> 
  filter(year == test_yr) |> 
  group_by(id) |> 
  summarise(pop = sum(pop)) |> 
  ungroup() |> 
  arrange(pop) |>
  mutate(npp_rnk = 1:n()) |> 
  select(id, npp_rnk)

rnk_diff <- cus_rnk |>
  left_join(npp_rnk, join_by("id")) |> 
  mutate(rnk_diff = cus_rnk - npp_rnk) |>
  arrange(rnk_diff)

test_that("test consistency of custom variant ranking v. NPP rank by LA", {
  expect_lte(
    max(abs(rnk_diff$rnk_diff)), 3L
    )
})


# B) test difference between population totals for variants that are in both custom vars and original snpp set by LA
cus_vars <- custom |> 
  filter(id %in% c("en_pph", "en_ppl")) |>
  mutate(data = map(data, \(x) x |> 
                      filter(year == test_yr) |> 
                      group_by(area_code, area_name) |> 
                      summarise(pop = sum(pop)) |> 
                      ungroup()
  )
  ) |> 
  unnest(data)

orig_snpp <- snpp_dat |>
  filter(ons_id %in% c(
    "var_proj_low_intl_migration",
    "var_proj_high_intl_migration")
  ) |> 
  mutate(data = map(data, \(x) x |> 
                      filter(year == test_yr) |>
                      group_by(area_code, area_name) |> 
                      summarise(pop = sum(pop)) |> 
                      ungroup()
  )
  ) |> 
  unnest(data)

var_diff <-cus_vars |> 
  left_join(orig_snpp, join_by("id" == "map_id", "area_code", "area_name")) |> 
  mutate(diff = (pop.x / pop.y - 1) * 100) |> 
  arrange(diff)

# plot differences
# ggplot(data = var_diff, aes(x = reorder(area_name, diff), y = diff, group = 1L)) +
#   geom_point() +
#   scale_x_discrete(name = NULL) +
#   scale_y_continuous(name = NULL) +
#   facet_wrap(vars(id)) +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(.8)))

test_that("test difference between variants that are in both custom variants and origianl SNPP set by LA", {
  expect_lte(
    max(abs(var_diff$diff)), 10
  )
})

