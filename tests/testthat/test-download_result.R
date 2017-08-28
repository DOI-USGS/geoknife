context("download and load")

gconfig(wps.url = "https://cida-test.er.usgs.gov/gdp/process/WebProcessingService")
test_that("download works", {
  testthat::skip_on_cran()
  cancel()
  fabric <- webdata('prism')
  times(fabric)[2] <- "1895-01-01"
  job <<- geoknife(stencil = c(-89,42), fabric = fabric, wait=TRUE)
  file = download(job)
  expect_true(file.exists(file))
})

test_that("load result works", {
  testthat::skip_on_cran()
  expect_is(result(job),'data.frame')
})

test_that("load result works from job id URL", {
  testthat::skip_on_cran()
  expect_is(result(id(job)),'data.frame')
})

test_that("download result works from job id URL", {
  testthat::skip_on_cran()
  expect_true(file.exists(download(id(job), overwrite = TRUE)))
})

test_that("download result works from job", {
  testthat::skip_on_cran()
  expect_true(file.exists(download(job, overwrite = TRUE)))
})