context("Test create webprocess object")
test_that("creation of webprocess object", {
  testthat::skip_on_cran()
  expect_is(webprocess(), "webprocess")
  wp <- webprocess(url = 'http://cida-test.er.usgs.gov/process/WebProcessingService')
  expect_is(wp, "webprocess")
})

context('test modifying webprocess object')

test_that("webprocess object is modified correctly", {
  testthat::skip_on_cran()
  wp <- webprocess(url = 'http://cida-test.er.usgs.gov/gdp/process/WebProcessingService')
  # test setting url also changes util
  expect_equal(slot(wp, 'UTILITY_URL'), 'http://cida-test.er.usgs.gov/gdp/utility/WebProcessingService')
  url(wp) = 'http://cida-test.er.usgs.gov/gdp/process/WebProcessingService'
  expect_equal(url(wp), url(wp <- webprocess(url = 'http://cida-test.er.usgs.gov/gdp/process/WebProcessingService')))

})

context('test get and set webprocess object')
test_that("webprocess object get and set", {
  testthat::skip_on_cran()
  wp <- webprocess()
  expect_is(url(wp),'character')
  expect_is(version(wp), 'character')
  expect_is(slot(wp, "processInputs"), 'list')
  
  
})

context('test pass through of webprocess')
test_that("test pass through of webprocess", {
  testthat::skip_on_cran()
  wp1 <- webprocess()
  wp2 <- webprocess(wp1, DATASET_URI = 'www.test.com')
  expect_null(inputs(wp1,'DATASET_URI')[[1]])
  expect_equal(inputs(wp2,'DATASET_URI')[[1]], 'www.test.com')
  expect_is(url(wp2),'character')
  expect_is(version(wp2), 'character')
  expect_is(slot(wp2, "processInputs"), 'list')

})

context('test pass through of webprocess inputs')
test_that('test pass through of webprocess inputs', {
  wp = webprocess(algorithm = list('OPeNDAP Subset'="gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm"), REQUIRE_FULL_COVERAGE = 'false')
  wp = initialize(wp)
  expect_equal(inputs(wp,'REQUIRE_FULL_COVERAGE')[[1]], 'false')
})
