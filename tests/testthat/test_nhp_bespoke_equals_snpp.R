
context("nhp custom snpp")

test_that("nhp bespoke snpp 90+ populations match snpp 90+ populations", {
  
  expect_equal(
    nhp_snpp_2018b$principal_proj |> 
      filter(age >= 90) |> 
      group_by(year) |> 
      summarise(pop = sum(pop)),
    snpp_2018b_dat$principal_proj |> 
      filter(age_group == "90 and over") |> 
      group_by(year) |> 
      summarise(pop = sum(pop))
    )
})

