context("Test set job state")


test_that("creating simple job", {
  cancel()
  testthat::skip_on_cran()
  stencil <- webgeom('state::Wisconsin')
  fabric <- webdata('prism')
  expect_equal(geoknife:::getJobState(), 'none')
})

test_that("can start first job",{
  testthat::skip_on_cran()
  stencil <- webgeom('state::Wisconsin')
  fabric <- webdata('prism')
  job <- geoknife(stencil, fabric)
  expect_equal(geoknife:::getJobState(), "ProcessStarted")
  
})

test_that("fail for second job",{
  testthat::skip_on_cran()
  stencil <- webgeom('state::Wisconsin')
  fabric <- webdata('prism')
  expect_error(geoknife(stencil, fabric)) #because is running.
  cancel()
  geoknife(stencil, fabric) #expect no error
  
})
