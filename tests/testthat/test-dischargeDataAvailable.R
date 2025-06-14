test_that("invalid search term returns NULL", {
  state = "WA"
  search_term <- "yikima"
  sites_df <- dischargeDataAvailable(state, search_term)
  expect_true(is.null(sites_df))
})

test_that("valid search term returns data.frame", {
  state = "WA"
  search_term <- "cedar"
  sites_df <- dischargeDataAvailable(state, search_term)
  expect_equal(class(sites_df), "data.frame")
})

test_that("valid search term returns data.frame with greater than 0 rows", {
  state = "WA"
  search_term = "yakima"
  sites_df <- dischargeDataAvailable(state, search_term)
  expect_true(nrow(sites_df) > 0)
})
