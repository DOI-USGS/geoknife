context("dodsReplace")


test_that("prism is replaced", {
  testthat::skip_on_cran()
  expect_equal(geoknife:::dodsReplace('https://cida.usgs.gov/thredds/dodsC/prism_v2'),'dods://cida.usgs.gov/thredds/dodsC/prism_v2')
})
