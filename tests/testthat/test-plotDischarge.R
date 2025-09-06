

test_that("Returns a ggplot2 object", {
  site_no <- "12484500" ## Yakima river at Umtanum
  plot <- plotDischarge(site_no)
  expect_equal(class(plot), c("gg", "ggplot"))
})
