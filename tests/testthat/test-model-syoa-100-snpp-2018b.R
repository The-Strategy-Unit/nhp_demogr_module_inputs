# README ----
# 1 test for each of the x4 snpp variants that the sum of modeled 90 to 100+
# population across all LAs is the same as the 90+ population for England
# from NPP
snpp_90 <- out_dat |>
  mutate(data = map(
    data, \(x) x |>
      filter(str_detect(area_code, "^E07", negate = TRUE)) |>
      filter(age >= 90) |>
      group_by(year) |>
      summarise(pop = sum(pop)) |>
      ungroup()
  )) |>
  unnest(data)

npp_90 <- npp_dat |>
  mutate(data = map(
    data, \(x) x |>
      filter(age_group %in% c(as.character(seq(90, 104)),
          "105 - 109", "110 and over"
        )
      ) |>
      filter(year %in% as.character(2018:2043)) |>
      group_by(year) |>
      summarise(pop = sum(pop)) |>
      ungroup()
  )) |>
  unnest(data)

test_that("modeled snpp variant 90-100+ populations match
  npp 90+ populations", {
    expect_lt(
      npp_90 |>
        inner_join(snpp_90, join_by("ons_id" == "ons_id.y", "year")) |>
        mutate(diff = pop.x - pop.y) |>
        summarise(diff = sum(abs(diff))) |>
        pull(diff), 1
    )
  }
)