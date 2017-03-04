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

context("geoknife convienence functions for job state")

test_that("check status of empty job", {
  testthat::skip_on_cran()
  expect_equal(check(geojob())$status, 'none')
  expect_equal(check(geojob())$statusType, 'none')
})

test_that("check status of an ID that is not valid", {
  testthat::skip_on_cran()
  expect_error(check(geojob(id='bad_id')), 'is not a valid geojob ID. Status cannot be checked')
})

test_that("check status of an ID that is incorrect", {
  testthat::skip_on_cran()
  fake.id <- 'bad_?id=asdf'
  status <- check(geojob(id=fake.id))
  expect_equal(status$status, "unknown")
  expect_equal(status$statusType, "unknown")
})

