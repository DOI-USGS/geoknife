context("Test setting of geoknife algorithms and process inputs")


test_that("webprocess can set algorithms", {
  testthat::skip_on_cran()
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
  testthat::skip_on_cran()
  states <- c("Colorado","Oregon","Wisconsin")
  wg <- webgeom(paste0('state::',paste(states,collapse = ',')))
  expect_equal(length(values(wg)), length(states))
})

context("geoknife sets stencil correctly")
test_that("geoknife sets stencil correctly", {
  testthat::skip_on_cran()
  job <- geoknife('HUC8::09020306', 'prism')
  expect_is(job, 'geojob')
  cancel(job)
  job <- geoknife(data.frame('point1'=c(-89, 46), 'point2'=c(-88.6, 45.2)), 'prism')
  expect_is(job, 'geojob')
  cancel(job)
})

context("geoknife w/ knife modified in line")
test_that("geoknife sets knife correctly", {
  testthat::skip_on_cran()
  job <- geoknife('HUC8::09020306', 'prism', wait=TRUE)
  expect_true(geoknife:::canStart())
})

