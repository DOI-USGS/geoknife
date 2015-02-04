context("Test setting of geoknife algorithms and process inputs")

gk <- geoknife()
algs <- getAlgorithms(gk)
algorithm <- list("Area Grid Statistics (weighted)"=
                   "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
dataset <- list('DATASET_ID'='Downward_longwave_radiation_flux_surface')
test_that("geoknife can set algorithms", {

	expect_error(setAlgorithm(gk)<-'bad.char')
	expect_error(setAlgorithm(gk)<-NULL)
	expect_error(setAlgorithm(gk)<-list('junk'='will break process'))
	
	setAlgorithm(gk) <- algorithm
	# test that it properly sets
	expect_equal(gk@algorithm[[1]],algorithm[[1]])

})
test_that("geoknife can set process inputs", {
	expect_is(gk, "geoknife")
	setAlgorithm(gk) <- algorithm
	expect_is(gk@processInputs,"list")
	expect_error(setProcessInputs(gk)<- 'character_input')
	# test that it properly sets
	setProcessInputs(gk)<- dataset
	expect_equal(gk@processInputs[names(dataset)][[1]],dataset[[1]])
})

test_that("geoknife can set WFS and WPS", {
  WFS = 'WFS_fake'
  setWFS(gk) <- WFS
  expect_equal(gk@WFS_URL, WFS)
  WPS = 'WPS_fake'
  setWPS(gk) <- WPS
  expect_equal(gk@WPS_URL, WPS)
})

test_that("geoknife can check empty process", {
  expect_equal(checkProcess(gk)$status,'none')
})