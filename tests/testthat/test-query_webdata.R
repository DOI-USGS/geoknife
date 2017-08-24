context("query for variables")

test_that("can't query for variables w/o a url", {
  testthat::skip_on_cran()
  expect_error(query(webdata(), 'variables'), 'url cannot be NA for fabric argument when querying for available variables')
})

context("query for times")

test_that("can't query for times w/o a variable", {
  testthat::skip_on_cran()
  wd <- webdata('prism')
  variables(wd) <- NA
  expect_error(query(wd, 'times'), "variables cannot be NA for fabric argument when querying for available time range")
})

test_that("query for times warns w/ multiple variables", {
  testthat::skip_on_cran()
  wd <- webdata('prism')
  variables(wd) <- query(wd, 'variables')
  expect_true(length(variables(wd)) > 1) # make sure this is actually being tested
  expect_warning(query(wd, 'times'), 'variables is > 1, using ')
})

test_that("query works for times with base dataset", {
  testthat::skip_on_cran()
  wd <- webdata('prism')
  times.out <- query(wd, 'times')
  expect_is(times.out, "POSIXct")
  expect_false(any(is.na(times.out)))
})

test_that("execute XML for getgridtimerange works", {
  wd <- readRDS("data/test_XML_wd.rds")
  wp <- readRDS("data/test_XML_wp.rds")
  xml <- geoknife:::make_getgridtimerange_execute_xml(wd, wp)
  
  fn <- "data/test_getgridtimerange.xml"
  expect_equal(xml,readChar(fn, file.info(fn)$size))
})

test_that("execute XML for getgridtimerange works", {
  wd <- readRDS("data/test_XML_wd.rds")
  wp <- readRDS("data/test_XML_wp.rds")
  xml <- geoknife:::make_listopendapgrids_execute_xml(wd, wp)
  
  fn <- "data/test_listopendapgrids.xml"
  expect_equal(xml,readChar(fn, file.info(fn)$size))
})

test_that("bad url query returns NAs", {
  testthat::skip_on_cran()
  wd <- webdata('prism', url='https://cida.usgs.gov/')
  suppressWarnings(times.out <- query(wd, 'times'))
  expect_true(all(is.na(times.out)))
})

