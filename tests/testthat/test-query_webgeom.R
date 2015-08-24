context("query webgeom")


test_that("query geoms", {
  expect_is(query(webgeom(), 'geoms'),'character')
  wg <- webgeom(geom = "sample:CONUS_states",
                attribute = "STATE",
                values = "New Hampshire")
  expect_is(query(wg, 'geoms'),'character')
  expect_error(query(webgeom(), 'attributes'))
  expect_error(query(webgeom(), 'values'))
})

test_that("query attributes", {
  wg <- webgeom(geom = "sample:CONUS_states",
              attribute = "STATE",
              values = "New Hampshire")
  expect_is(query(wg, 'attributes'),'character')
})

test_that("query values", {
  wg <- webgeom(geom = "sample:CONUS_states",
                attribute = "STATE",
                values = "New Hampshire")
  expect_is(query(wg, 'values'),'character')
})