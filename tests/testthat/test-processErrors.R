context("test failing request")

cancel()
stencil <- webgeom('state::NH')
fabric <- webdata('prism', variables = 'wrong')


test_that("java stack failure", {
  
  job <- geoknife(stencil, fabric, waitUntilFinished = TRUE)
  expect_equal(geoknife:::getJobState(), "ProcessFailed")
})

context("testing required field as NA in webdata")

cancel()
test_that("required field as NA in webdata", {
  variables(fabric) <- as.character(NA)
  expect_error(geoknife(stencil, fabric))
})