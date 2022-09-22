
test_that("custom sub-national vars total equal npp vars total", {
  expect_equal(
    map(ls_snpp_custom_vars, ~ . |> group_by(year) |> summarise(pop = sum(pop))),
    map(npp_2018b_dat, ~ . |> filter(year %in% as.character(2018:2043)) |> group_by(year) |> summarise(pop = sum(pop))) 
  )
})

