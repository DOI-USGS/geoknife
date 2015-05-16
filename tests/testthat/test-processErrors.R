context("test failing request")

cancel()
stencil <- webgeom('state::NH')
fabric <- webdata('prism', variables = 'wrong')


test_that("java stack failure", {
  
  expect_equal(geoknife:::getJobState(), 'none')
  job <- geoknife(stencil, fabric, waitUntilFinished = TRUE)
  expect_equal(geoknife:::getJobState(), "ProcessFailed")
})
