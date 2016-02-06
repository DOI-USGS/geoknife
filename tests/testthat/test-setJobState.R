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
  expect_true(geoknife:::getJobState() %in% c("ProcessStarted","Process successful"))
  
})

test_that("fail for second job",{
  testthat::skip_on_cran()
  stencil <- webgeom('state::Wisconsin')
  fabric <- webdata('prism')
  geoknife:::setJobState("ProcessStarted")
  expect_error(geoknife(stencil, fabric)) #because is running.
  cancel()
  expect_is(geoknife(stencil, fabric),'geojob') #expect no error
  
})
