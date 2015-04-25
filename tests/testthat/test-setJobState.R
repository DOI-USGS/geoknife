context("Test set job state")

stencil <- webgeom('state:NH')
fabric <- webdata('prism')

test_that("creating simple job", {
  
  expect_equal(geoknife:::getJobState(), 'none')
})

test_that("can start first job",{
  job <- geoknife(stencil, fabric)
  expect_equal(geoknife:::getJobState(), 'running')
  
})

test_that("fail for second job",{
  expect_warning(geoknife(stencil, fabric)) #because is running.
  geoknife:::setJobState('none') # set it back to open
  geoknife(stencil, fabric) #expect no error
  
})
