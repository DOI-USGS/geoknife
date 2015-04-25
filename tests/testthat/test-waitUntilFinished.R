context("Test wait until finished")

stencil <- webgeom('state:NH')
fabric <- webdata('prism')

test_that("creating simple job", {
  geoknife:::setJobState('none')
  geoknife(stencil, fabric, waitUntilFinished = TRUE)
  expect_equal(geoknife:::getJobState(), "Process successful")
  expect_true(geoknife:::canStart())
  geoknife(stencil, fabric, waitUntilFinished = FALSE)
  expect_false(geoknife:::getJobState() == "Process successful")
})