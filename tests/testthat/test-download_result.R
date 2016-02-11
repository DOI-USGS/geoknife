context("download and load")

gconfig(wps.url = "http://cida-test.er.usgs.gov/gdp/process/WebProcessingService")
test_that("download works", {
  testthat::skip_on_cran()
  cancel()
  job <<- geoknife(stencil = c(-89,42), fabric = 'prism', wait=TRUE)
  file = download(job)
  expect_true(file.exists(file))
})

test_that("load result works", {
  testthat::skip_on_cran()
  expect_is(result(job),'data.frame')
})