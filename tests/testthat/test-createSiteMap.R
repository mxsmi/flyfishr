test_that("check that createSiteMap returns a leaflet object", {
  site_no <- "12484500" ## Yakima river at Umtanum
  map <- createSiteMap(site_no)
  expect_equal(class(map), c("leaflet", "htmlwidget"))
})
