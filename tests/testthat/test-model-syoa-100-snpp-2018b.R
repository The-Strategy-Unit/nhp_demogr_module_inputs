
snpp_90 <- out_dat |> 
  mutate(data = map(data, \(x)
                    x |>
                      filter(str_detect(area_code, "^E07", negate = TRUE)) |> 
                      filter(age >= 90) |> 
                      group_by(year) |> 
                      summarise(pop = sum(pop)) |> 
                      ungroup()
  )) |> 
  unnest(data)

npp_90 <- npp_dat |> 
  filter(age %in% c(as.character(seq(90, 104)), "105 - 109", "110 and over")) |> 
  filter(year <= 2043) |> 
  group_by(id, year) |> 
  summarise(pop = sum(pop)) |> 
  ungroup()

test_that("spread snpp 90+ populations match original npp 90+ populations by variant", {
  expect_lt(npp_90 |> 
              inner_join(snpp_90, join_by("id" == "map_id", "year")) |> 
              mutate(diff = pop.x - pop.y) |> 
              summarise(diff = sum(abs(diff))) |> 
              pull(diff)
    , 1)
})

