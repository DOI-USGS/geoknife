context("Test set job state")

cancel()
stencil <- webgeom('state::Wisconsin')
fabric <- webdata('prism')

test_that("creating simple job", {
  
  expect_equal(geoknife:::getJobState(), 'none')
})

test_that("can start first job",{
  job <- geoknife(stencil, fabric)
  expect_equal(geoknife:::getJobState(), "ProcessStarted")
  
})

test_that("fail for second job",{
  expect_error(geoknife(stencil, fabric)) #because is running.
  cancel()
  geoknife(stencil, fabric) #expect no error
  
})
