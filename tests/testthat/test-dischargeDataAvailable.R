

test_that("valid search term returns data.frame", {
  state = "TX"
  sites_df <- dischargeDataAvailable(state)
  expect_equal(class(sites_df), "data.frame")
})

test_that("valid search term returns data.frame with greater than 0 rows", {
  state = "MA"
  sites_df <- dischargeDataAvailable(state)
  expect_true(nrow(sites_df) > 0)
})
