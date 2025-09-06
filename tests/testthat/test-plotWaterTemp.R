

test_that("Returns a ggplot object", {
  site_no <- "12484500" ## Yakima river at Umtanum
  plot <- plotWaterTemp(site_no)
  expect_equal(class(plot), c("gg", "ggplot"))
})
