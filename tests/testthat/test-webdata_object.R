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

test_that("create webdata from geojob", {
  xml <- system.file("extdata/state_webgeom_post.xml", 
                               package = "geoknife")
  geojob <- geojob(xml=xml)
  wd <- webdata(geojob)
  expect_equal(length(times(wd)), 2)
  expect_is(url(wd), "character")
  expect_is(variables(wd), "character")
  expect_gt(length(variables(wd)), 0)
  
  #test XML missing time slots
  testthat::skip_on_cran()
  noTimesJob <- geojob("data/test_notimesjob.xml")
  wd <- webdata(noTimesJob)
  expect_equal(length(times(wd)), 2)
  expect_is(times(wd), "POSIXct")
  
  #make sure times aren't dropped
  wd <- webdata(geojob(xml = "data/test_keeptimes.xml"))
  n <- nchar(as.character(times(wd)))
  expect_true(all(n > 10))
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
  expect_equal(as.numeric(times(wd)[1]-as.POSIXct('2000-01-01')), 0)
  url(wd) <- 'www.badurlppppp.com'
  expect_is(url(wd), "character")
})

