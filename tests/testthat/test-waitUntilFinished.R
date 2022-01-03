context("Test wait until finished")

default.sleep <- geoknife:::gconfig('sleep.time')
geoknife:::gconfig(sleep.time=0.1)

test_that("creating simple job", {
  testthat::skip_on_cran()
  stencil <- readRDS("data/test_webgeom_WI.rds")
  fabric <- readRDS("data/test_webdata_fabric.rds")
  geoknife:::setJobState('none')
  geoknife(stencil, fabric, wait = TRUE)
  
  expect_equal(geoknife:::getJobState(), "Process successful")
  expect_true(geoknife:::canStart())
  geoknife(stencil, fabric, wait = FALSE)
  expect_false(geoknife:::getJobState() == "Process successful")
  cancel()
})

context("Test re-enter wait loop")
test_that("creation of webprocess object", {
  testthat::skip_on_cran()
  fabric <- webdata('prism',times <- c('1895-01-01T00:00:00Z', '1895-01-01T00:00:00Z')) # should take longer than a re-check
  cancel()
  job <- geoknife(stencil = c(-89,42), fabric, wait=FALSE)
  expect_equal(2+2, 4)
  expect_true(running(job))
  m <- capture_messages(wait(job, show.progress = TRUE))
  expect_false(running(job))
  
  expect_equal(m, "Process Accepted\n")

})

geoknife:::gconfig(sleep.time=default.sleep)
