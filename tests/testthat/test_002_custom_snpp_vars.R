

test_that("test consistency of custom variant order v. NPP order", {
  expect_lte(
    max(abs(
      rnk_diffs |> 
        pull(diff_rnk)
    )), 2L
  )
})

test_that("test consistency of custom variant order v. NPP order", {
  expect_gt(
    max(abs(
      rnk_diffs |> 
        group_by(abs_diff_rnk = abs(diff_rnk)) |> 
        summarise(wght_diff = sum(n)) |> 
        filter(abs_diff_rnk == 0) |> 
        pull(wght_diff)
    )), .8 * (18 * 326)
  )
})

test_that("test difference between variants that are in both SNPP and NPP, by LA", {
  expect_lte(
    max(abs(
      cf_vars |> 
        pull(p_high)
    )), 10L
  )
  expect_lte(
    max(abs(
      cf_vars |> 
        pull(p_low)
    )), 10L
  )
  expect_lte(
    cf_vars |> 
      summarise(p_high = mean(abs(p_high)))
    , 1L
  )
  expect_lte(
    cf_vars |> 
      summarise(p_low = mean(abs(p_low)))
    , 1L
  )
})
