context("Test create webdata object")
test_that("creation of webdata object", {
  testthat::skip_on_cran()
  expect_is(webdata(), "webdata")
  expect_is(webdata('prism'), "webdata")
  wd <- webdata('prism',times = as.POSIXct(c('2001-01-01','2002-02-05')))
  expect_is(wd, "webdata")
  times(wd)[1] <- as.POSIXct('2000-01-01')
  expect_is(wd, "webdata")
})

context("Test getting fields of webdata object")
test_that("getters work", {
  testthat::skip_on_cran()
  wd <- webdata('prism',times = as.POSIXct(c('2001-01-01','2002-02-05')))
  
  expect_is(times(wd), "POSIXct")
  expect_is(url(wd), "character")
})

context("Test setting fields of webdata object")
test_that("setters work", {
  testthat::skip_on_cran()
  wd <- webdata('prism',times = as.POSIXct(c('2001-01-01','2002-02-05')))
  times(wd)[1] <- as.POSIXct('2000-01-01')
  expect_equal(times(wd)[1],as.POSIXct('2000-01-01'))
  url(wd) <- 'www.badurlppppp.com'
  expect_is(url(wd), "character")
})