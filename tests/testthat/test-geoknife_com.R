context("Test geoknife connection to GDP")



test_that("geoknife and algorithms return proper data", {
  testthat::skip_on_cran()
  wp <- webprocess()
  expect_is(wp, "webprocess")
  expect_is(wp@processInputs, "list")
})
