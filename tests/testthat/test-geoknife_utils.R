context("geoknife utils")


test_that("verbose", {
  testthat::skip_on_cran()
  expect_null(geoknife:::gverbose())
  gconfig(verbose=TRUE)
  expect_is(geoknife:::gverbose(), 'request')
  gconfig(verbose=FALSE)
  
})

test_that("error on url", {
  testthat::skip_on_cran()
  expect_error(geoknife:::retryVERB(httr::GET('bad.url.html')), "Couldn't resolve host name")
})