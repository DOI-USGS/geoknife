context("Test setting of geoknife algorithms and process inputs")

gk <- geoknife()
algs <- getAlgorithms(gk)

test_that("geoknife can set algorithms", {
	set.bad <- function(gk,val){
		setAlgorithm(gk)<-val
	}
	expect_error(set.bad(gk,val='bad.char'))
	expect_error(set.bad(gk,val=NULL))
	expect_error(set.bad(gk,val=list('junk'='will break process')))

})
#test_that("geoknife can set process inputs", {