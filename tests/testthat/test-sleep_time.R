context("Test sleep params")
test_that("Test sleep params", {
  testthat::skip_on_cran()
  default.sleep <- geoknife:::gconfig('sleep.time')
  wd <- readRDS("data/test_webprocess_knife.rds")
  expect_equal(wd@sleep.time, default.sleep)
  wd <- readRDS("data/test_webprocess_sleep-plus-five.rds")
  expect_false(wd@sleep.time == default.sleep)
})

test_that("Test set sleep global and param", {
  testthat::skip_on_cran()
  default.sleep <- geoknife:::gconfig('sleep.time')
  geoknife:::gconfig('sleep.time'=default.sleep+5)
  wd <- webprocess() # tests on init.
  expect_equal(wd@sleep.time, default.sleep+5)
  wd <- webprocess(sleep.time = default.sleep)
  expect_equal(wd@sleep.time, default.sleep)
})