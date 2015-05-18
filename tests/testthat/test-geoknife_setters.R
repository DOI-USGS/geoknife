context("Test setting of geoknife algorithms and process inputs")


test_that("webprocess can set algorithms", {
  wp <- webprocess()
  algs <- query(wp, 'algorithms')
  expect_error(algorithm(wp)<-'bad.char')
	expect_error(algorithm(wp)<-NULL)
	expect_error(algorithm(wp)<-list('junk'='will break process'))
	
	algorithm(wp) <- algs[1]
	# test that it properly sets
	expect_equal(algorithm(wp), algs[1])

})

context("Test setting of webgeom simple sets")
test_that("webgeom can set states", {
  states <- c("CO","OR","CN")
  expect_error(webgeom(paste0('state::',paste(states,collapse = ','))))
  states <- c("CO","OR","CT")
  wg <- webgeom(paste0('state::',paste(states,collapse = ',')))
  expect_equal(length(values(wg)), length(states))
})
# 
# gk <- geoknife()
# algs <- getAlgorithms(gk)
# algorithm <- list("Area Grid Statistics (weighted)"=
#                    "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
# dataset <- list('DATASET_ID'='Downward_longwave_radiation_flux_surface')
# test_that("geoknife can set algorithms", {
# 
# 	expect_error(setAlgorithm(gk)<-'bad.char')
# 	expect_error(setAlgorithm(gk)<-NULL)
# 	expect_error(setAlgorithm(gk)<-list('junk'='will break process'))
# 	
# 	setAlgorithm(gk) <- algorithm
# 	# test that it properly sets
# 	expect_equal(gk@algorithm[[1]],algorithm[[1]])
# 
# })
# test_that("geoknife can set process inputs", {
# 	expect_is(gk, "geoknife")
# 	setAlgorithm(gk) <- algorithm
# 	expect_is(gk@processInputs,"list")
# 	expect_error(setProcessInputs(gk)<- 'character_input')
# 	# test that it properly sets
# 	setProcessInputs(gk)<- dataset
# 	expect_equal(gk@processInputs[names(dataset)][[1]],dataset[[1]])
# })
# 
# test_that("geoknife can set WFS and WPS", {
#   WFS = 'WFS_fake'
#   setWFS(gk) <- WFS
#   expect_equal(gk@WFS_URL, WFS)
#   WPS = 'WPS_fake'
#   setWPS(gk) <- WPS
#   expect_equal(gk@WPS_URL, WPS)
# })
# 
# test_that("geoknife can check empty process", {
#   expect_equal(checkProcess(gk)$status,'none')
#   gk@processID = 'www.fakeurlbreakprocess.com'
#   expect_equal(checkProcess(gk)$status,'unknown')
#   
# })