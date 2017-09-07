context("shorthand webprocess()")


test_that("character to knife", {
  testthat::skip_on_cran()
  expect_is(webprocess('subset'), 'webprocess')
  expect_error(webprocess('not a knife'))
  
})

test_that("built in knifes are algorithms on prod", {
  testthat::skip_on_cran()
  web.knives <- unname(unlist(readRDS("data/test_webprocess_algorithms_prod.rds")))
  pkg.knives <- unlist(unname(lapply(geoknife:::getKnives(),function(x) x[[1]][[1]])))
  
  expect_true(all(pkg.knives %in% web.knives))
})

test_that('multi-args work with shorthand knife',{
  wp <- webprocess('subset', OUTPUT_TYPE='geotiff') # this makes two describe process requests!!! HUH?
  expect_is(wp, 'webprocess')
  expect_equal(inputs(wp)$OUTPUT_TYPE, 'geotiff')
  
})