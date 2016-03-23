context("shorthand webprocess()")


test_that("character to knife", {
  testthat::skip_on_cran()
  expect_is(webprocess('subset'), 'webprocess')
  expect_error(webprocess('not a knife'))
  
})

test_that("built in knifes are algorithms", {
  testthat::skip_on_cran()
  wps.url <- gconfig('wps.url')
  gconfig('wps.url'="http://cida.usgs.gov/gdp/process/WebProcessingService")
  web.knives <- unname(unlist(query(webprocess(),'algorithms')))
  pkg.knives <- unlist(unname(lapply(geoknife:::getKnives(),function(x) x[[1]][[1]])))
  
  expect_true(all(pkg.knives %in% web.knives))
  gconfig('wps.url'=wps.url)
})

test_that('multi-args work with shorthand knife',{
  wp <- webprocess('subset', OUTPUT_TYPE='geotiff')
  expect_is(wp, 'webprocess')
  expect_equal(inputs(wp)$OUTPUT_TYPE, 'geotiff')
  
})