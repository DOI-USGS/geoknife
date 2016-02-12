context("Test wait until finished")


test_that("creating simple job", {
  testthat::skip_on_cran()
  stencil <- webgeom('state::Wisconsin')
  fabric <- webdata('prism')
  geoknife:::setJobState('none')
  geoknife(stencil, fabric, wait = TRUE)
  
  expect_equal(geoknife:::getJobState(), "Process successful")
  expect_true(geoknife:::canStart())
  geoknife(stencil, fabric, wait = FALSE)
  expect_false(geoknife:::getJobState() == "Process successful")
})

context("Test re-enter wait loop")
test_that("creation of webprocess object", {
  testthat::skip_on_cran()
  fabric <- webdata('prism',times <- c('1895-01-01T00:00:00Z', '1999-01-01T00:00:00Z')) # should take longer than a re-check
  cancel()
  job <- geoknife(stencil = c(-89,42), fabric, wait=FALSE)
  expect_equal(2+2, 4)
  expect_true(running(job))
  wait(job)
  expect_false(running(job))
})
