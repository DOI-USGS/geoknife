context("test failing request")




test_that("java stack failure", {
  testthat::skip_on_cran()
  cancel()
  stencil <- webgeom('state::Wisconsin')
  fabric <- webdata('prism', variables = 'wrong')
  job <- geoknife(stencil, fabric, wait = TRUE)
  expect_equal(geoknife:::getJobState(), "ProcessFailed")
})

context("testing required field as NA in webdata")


test_that("required field as NA in webdata", {
  testthat::skip_on_cran()
  cancel()
  stencil <- webgeom('state::Wisconsin')
  fabric <- webdata('prism', variables = 'wrong')
  variables(fabric) <- as.character(NA)
  expect_error(geoknife(stencil, fabric))
})