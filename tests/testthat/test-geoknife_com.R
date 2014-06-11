context("Test geoknife connection to GDP")

gk <- geoknife()
algs <- getAlgorithms(gk)

test_that("geoknife and algorithms return proper data", {
  expect_is(gk, "geoknife")
  expect_is(algs, "list")
})
