context("Test create webprocess object")
test_that("creation of webprocess object", {
  expect_is(webprocess(), "webprocess")
  wp <- webprocess(url = 'http://cida-test.er.usgs.gov/process/WebProcessingService')
  expect_is(wp, "webprocess")
})

context('test modifying webprocess object')

test_that("webprocess object is modified correctly", {
  wp <- webprocess(url = 'http://cida-test.er.usgs.gov/gdp/process/WebProcessingService')
  # test setting url also changes util
  expect_equal(slot(wp, 'UTILITY_URL'), 'http://cida-test.er.usgs.gov/gdp/utility/WebProcessingService')
  url(wp) = 'http://cida-test.er.usgs.gov/gdp/process/WebProcessingService')
  expect_equal(url(wp), url(wp <- webprocess(url = 'http://cida-test.er.usgs.gov/gdp/process/WebProcessingService')))

})

context('test get and set webprocess object')
test_that("webprocess object get and set", {
  wp <- webprocess()
  expect_is(url(wp),'character')
  expect_is(version(wp), 'character')
  expect_is(slot(wp, "processInputs"), 'list')
  
  
})