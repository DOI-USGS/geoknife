context("Test geoknife connection to GDP")

wp <- webprocess()

test_that("geoknife and algorithms return proper data", {
  expect_is(wp, "webprocess")
  expect_is(wp@processInputs, "list")
})
