context("download and load")

test_that("download from geojob works", {
  testthat::skip_on_cran()
  cancel()
  fabric <- readRDS("data/test_webdata_fabric.rds")
  job <<- geoknife(stencil = c(-89,42), fabric = fabric, wait=TRUE)
  file = download(job)
  expect_true(file.exists(file))
})

test_that("load result works from geojob object.", {
  testthat::skip_on_cran()
  expect_is(result(job),'data.frame') 
})

test_that("load result works from job id only", {
  testthat::skip_on_cran()
  expect_is(result(id(job)),'data.frame')
})

# this test is duplicative and just takes extra time.
# test_that("download result works from job id URL", {
#   testthat::skip_on_cran()
#   expect_true(file.exists(download(id(job), overwrite = TRUE)))
# }) 

test_that("download result overwrite works", {
  testthat::skip_on_cran()
  expect_true(file.exists(download(job, overwrite = TRUE)))
})