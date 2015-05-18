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
