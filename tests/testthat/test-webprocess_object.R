context('test modifying webprocess object')

test_that("webprocess object is modified correctly", {
  testthat::skip_on_cran()
  wp <- readRDS("data/test_webprocess_knife.rds")
  # test setting url also changes util
  url(wp) <- 'https://cida-test.er.usgs.gov/gdp/process/WebProcessingService'
  expect_equal(slot(wp, 'UTILITY_URL'), "https://cida-test.er.usgs.gov/gdp/utility/WebProcessingService")
  url(wp) <- 'https://cida.usgs.gov/gdp/process/WebProcessingService'
  expect_equal(url(wp), url(wp <- webprocess(url = 'https://cida.usgs.gov/gdp/process/WebProcessingService')))

})

context('test get and set webprocess object')
test_that("webprocess object get and set", {
  testthat::skip_on_cran()
  wp <- readRDS("data/test_webprocess_knife.rds")
  expect_is(url(wp),'character')
  expect_is(version(wp), 'character')
  expect_is(slot(wp, "processInputs"), 'list')
  
  
})

context('test pass through of webprocess')

context('test pass through of webprocess inputs')
test_that('test pass through of webprocess inputs', {
  testthat::skip_on_cran()
  wp = webprocess(algorithm = list('OPeNDAP Subset'="gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm"), REQUIRE_FULL_COVERAGE = 'false')
  wp = initialize(wp)
  expect_equal(inputs(wp,'REQUIRE_FULL_COVERAGE')[[1]], 'false')
  wp = initialize(wp, OUTPUT_TYPE='geotiff', wait=TRUE)
  expect_equal(inputs(wp,'OUTPUT_TYPE')[[1]], 'geotiff')
  expect_true(wp@wait)
})

context("valid wps url")

test_that("error is thrown for non-WPS", {
  expect_warning(webprocess(url='http://www.google.com'))
})

context("can't set read only process inputs")

test_that("error is thrown read-only set on initialize", {
  skip_on_cran()
  expect_null(webprocess(TIME_START='1990-01-01T00:00:000Z'))
})

test_that("error is thrown read-only set on initialize w/ multiple vars", {
  skip_on_cran()
  expect_null(webprocess(TIME_START='1990-01-01T00:00:000Z', wait=TRUE))
})

test_that("error is thrown on job start for read only vars", {
  skip_on_cran()
  expect_null(job <- geoknife(stencil = c(-89,42), fabric = 'prism', TIME_START='1990-01-01T00:00:000Z', wait=TRUE))
  expect_null(job <- geoknife(stencil = c(-89,42), fabric = 'prism', TIME_START='1990-01-01T00:00:000Z'))
})

context("create webprocess from geojob")
# gconfig(wps.url = "https://cida-test.er.usgs.gov/gdp/process/WebProcessingService")
test_that("given a geojob, we can create a webprocess", {
  testthat::skip_on_cran()
  xml <- system.file("extdata/state_webgeom_post.xml", 
                               package = "geoknife")
  geojob <- geojob(xml=xml)
  webprocess <- webprocess(geojob)
  expect_equal(algorithm(webprocess), list("Area Grid Statistics (weighted)"="gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm"))
  expect_equal(version(webprocess), "1.0.0")
  # expect_equal(url(webprocess), "https://cida-test.er.usgs.gov/gdp/process/WebProcessingService")
  expect_equal(length(inputs(webprocess)), 11)
  expect_equal(inputs(webprocess)$`DATASET_ID`, "ppt")
})
