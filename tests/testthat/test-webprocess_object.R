context("Test create webprocess object")
test_that("creation of webprocess object", {
  expect_is(webprocess(), "webprocess")
  wp <- webprocess(url = 'cida-test.er.usgs.gov/process/WebProcessingService')
  expect_is(wp, "webprocess")
})

context('test modifying webprocess object')

test_that("webprocess object is modified correctly", {
  wp <- webprocess(url = 'cida-test.er.usgs.gov/process/WebProcessingService')
  # test setting url also changes util
  expect_equal(slot(wp, 'UTILITY_URL'), 'cida-test.er.usgs.gov/utility/WebProcessingService')
  
})