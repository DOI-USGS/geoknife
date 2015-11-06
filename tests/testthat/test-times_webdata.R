context("Test time settings for inputs")


test_that("check using a POSIX, and that the tz is honored and converted", {
  t <- geoknife:::geotime(as.POSIXct("2012-11-04",tz="America/Chicago"))
  expect_equal(attr(t, 'tzone'), "UTC")
  expect_is(t, 'POSIXct')
})

test_that("check using a POSIX w/o tz, it gets UTC", {
  t <- geoknife:::geotime(as.POSIXct("2012-11-04"))
  expect_equal(attr(t, 'tzone'), "UTC")
  expect_is(t, 'POSIXct')
})

test_that("check using a string, it gets UTC", {
  t <- geoknife:::geotime("2012-11-04")
  expect_equal(attr(t, 'tzone'), "UTC")
  expect_is(t, 'POSIXct')
  
  expect_equal(t, geoknife:::geotime(as.POSIXct("2012-11-04")))
  
  
})

