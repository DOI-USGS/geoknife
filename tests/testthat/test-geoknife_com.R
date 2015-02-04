context("Test geoknife connection to GDP")
library(methods)
gk <- geoknife()
algs <- getAlgorithms(gk)

test_that("geoknife and algorithms return proper data", {
  expect_is(gk, "geoknife")
  expect_is(algs, "list")
})
