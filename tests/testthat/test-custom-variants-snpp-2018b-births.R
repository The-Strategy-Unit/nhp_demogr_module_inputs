# README ----
# 1 rank custom variants by total births within each LA (for year == test_yr)
# and test against ranking of npp variants for total births in England
# pick a year to test
test_yr <- "2035"

cus_rnk <- custom |>
  mutate(data = map(
    data, \(x) x |>
      filter(year == test_yr) |>
      filter(str_detect(area_code, "^E07", negate = TRUE)) |>
      group_by(area_code, area_name) |>
      summarise(bths = sum(bths)) |>
      ungroup()
  )) |>
  unnest(data) |>
  group_by(area_code, area_name) |>
  arrange(bths) |>
  mutate(cus_rnk = seq_len(n())) |>
  ungroup()

npp_rnk <- npp_dat |>
  unnest(c(data)) |>
  filter(year == test_yr) |>
  group_by(ons_id) |>
  summarise(bths = sum(bths)) |>
  ungroup() |>
  arrange(bths) |>
  mutate(npp_rnk = seq_len(n())) |>
  select(ons_id, npp_rnk)

rnk_diff <- cus_rnk |>
  left_join(npp_rnk, join_by("ons_id")) |>
  mutate(rnk_diff = cus_rnk - npp_rnk) |>
  arrange(rnk_diff)

test_that("test consistency of custom variant ranking within LAs against NPP
  ranking", {
    expect_lte(
      max(abs(rnk_diff$rnk_diff)), 3L
    )
  }
)

# 2 compare total births by LA (for year == test_yr) for custom variants that
# also appear in original snpp variant set
test_yr <- "2040"

cus_vars <- custom |>
  filter(ons_id %in% c("npp_pph", "npp_ppl")) |>
  mutate(data = map(
    data, \(x) x |>
      filter(year == test_yr) |>
      group_by(area_code, area_name) |>
      summarise(bths = sum(bths)) |>
      ungroup()
  )) |>
  unnest(data)

orig_snpp <- snpp_dat |>
  filter(ons_id %in% c(
    "var_proj_low_intl_migration",
    "var_proj_high_intl_migration"
  )) |>
  mutate(data = map(
    data, \(x) x |>
      filter(year == test_yr) |>
      group_by(area_code, area_name) |>
      summarise(bths = sum(bths)) |>
      ungroup()
  )) |>
  unnest(data) |>
  left_join(
    lookup_proj_var |>
      select(ons_id, id),
    join_by("ons_id")
  )

var_diff <- cus_vars |>
  left_join(
    lookup_proj_var |>
      select(ons_id, id),
    join_by("ons_id")
  ) |>
  left_join(orig_snpp, join_by("id", "area_code", "area_name")) |>
  mutate(diff = (bths.x / bths.y - 1) * 100) |>
  arrange(diff)

# plot differences
p <- var_diff |>
  mutate(area_name = tidytext::reorder_within(area_name, diff, id)) |>
  ggplot(aes(x = reorder(area_name, diff), y = diff, group = 1L)) +
  geom_point() +
  scale_x_reordered(name = NULL) +
  scale_y_continuous(name = NULL) +
  facet_wrap(vars(id)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = rel(.8)))

ggsave("test_custom_variants_snpp_births.png", plot = p, path = here("figures"))

test_that("test difference by LA for custom variants that also appear in
  original snpp variant set", {
    expect_lte(
      mean(abs(var_diff$diff)), 2
    )
  }
)