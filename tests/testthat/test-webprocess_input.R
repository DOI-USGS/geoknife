context("Test process input setters")


test_that("process inputs initialize with defaults", {
  wp <- webprocess()
  expect_is(wp, 'webprocess')
  expect_is(inputs(wp), 'list')
})

test_that("setting with optional arguments is possible",{
  
  wp <- webprocess(DELIMITER = 'TAB')
  wp2 <- webprocess()
  expect_true(inputs(wp, 'DELIMITER')[[1]] !=  inputs(wp2, 'DELIMITER')[[1]])
  expect_is(inputs(wp), 'list')
  inputs(wp) <- list(DELIMITER = 'COMMA', SUMMARIZE_FEATURE_ATTRIBUTE = 'false')
  expect_equal(inputs(wp, 'DELIMITER'),inputs(wp2, 'DELIMITER'))
})

test_that("get inputs works as expected",{
  
  wp <- webprocess(DELIMITER = 'TAB')
  expect_equal(inputs(wp, "DELIMITER")[[1]], 'TAB')
  expect_equal(length(inputs(wp, "DELIMITER")), 1)
  expect_equal(length(inputs(wp, "DELIMITER", "SUMMARIZE_FEATURE_ATTRIBUTE")), 2)
})

test_that("reseting algorithm sets inputs back to defaults",{
  wp <- webprocess(DELIMITER = 'TAB', SUMMARIZE_FEATURE_ATTRIBUTE = 'false', wait = TRUE, STATISTICS = "MEAN")
  expect_equal(inputs(wp, 'DELIMITER')[[1]], 'TAB')
  algorithm(wp) <- query(wp, 'algorithms')[1]
  algorithm(wp) <- list('Area Grid Statistics (unweighted)'="gov.usgs.cida.gdp.wps.algorithm.FeatureGridStatisticsAlgorithm")
  expect_equal(inputs(wp), inputs(webprocess()))
})

test_that("can use multiple dataset variables",{
  fabric <- webdata(url = 'http://cida.usgs.gov/thredds/dodsC/iclus/hc')
  variables(fabric) <- c('housing_classes_iclus_a1_2010', 'housing_classes_iclus_a1_2100')
  cancel()
  knife <- webprocess(algorithm = list('Categorical Coverage Fraction'="gov.usgs.cida.gdp.wps.algorithm.FeatureCategoricalGridCoverageAlgorithm"))
  expect_is(geoknife(stencil = 'state::Wisconsin', fabric, knife),'geojob')
})