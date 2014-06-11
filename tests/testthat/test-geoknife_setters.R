context("Test setting of geoknife algorithms and process inputs")

gk <- geoknife()
algs <- getAlgorithms(gk)

test_that("geoknife can set algorithms", {
	set.bad <- function(gk){
		setAlgorithm(gk)<-'bad.char'
	}
	expect_error(set.bad(gk))

})
#test_that("geoknife can set process inputs", {