context("query webgeom")


test_that("query geoms", {
  testthat::skip_on_cran()
  expect_is(query(webgeom(), 'geoms'),'character')
  wg <- webgeom(geom = "sample:CONUS_states",
                attribute = "STATE",
                values = "New Hampshire")
  expect_is(query(wg, 'geoms'),'character')
  expect_error(query(webgeom(), 'attributes'))
  expect_error(query(webgeom(), 'values'))
})

test_that("query attributes", {
  testthat::skip_on_cran()
  wg <- webgeom(geom = "sample:CONUS_states",
              attribute = "STATE",
              values = "New Hampshire")
  expect_is(query(wg, 'attributes'),'character')
})

test_that("query values", {
  testthat::skip_on_cran()
  wg <- webgeom(geom = "sample:CONUS_states",
                attribute = "STATE",
                values = "New Hampshire")
  expect_is(query(wg, 'values'),'character')
})

test_that("query values returns only unique", {
  expect_false(any(
    duplicated(
      query(webgeom(geom="sample:Counties" , attribute = "STATE_FIPS"), 'values'))
    )
  )
})

context("Create WFS post XML works")
test_that("two states", {
  wg <- readRDS("data/test_wfsgetfeature_wg.rds")
  xml <- geoknife:::wfsFilterFeatureXML(wg)

  fn <- "data/test_wfsgetfeature.xml"
  expect_equal(xml, gsub("\r", "", readChar(fn, file.info(fn)$size)))
})