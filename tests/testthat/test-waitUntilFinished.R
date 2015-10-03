context("Test wait until finished")

stencil <- webgeom('state::Wisconsin')
fabric <- webdata('prism')

test_that("creating simple job", {
  testthat::skip_on_cran()
  geoknife:::setJobState('none')
  geoknife(stencil, fabric, wait = TRUE)
  expect_equal(geoknife:::getJobState(), "Process successful")
  expect_true(geoknife:::canStart())
  geoknife(stencil, fabric, wait = FALSE)
  expect_false(geoknife:::getJobState() == "Process successful")
})