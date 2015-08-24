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

test_that("changing version",{
  wp <- webprocess()
  v1 = version(wp)
  version(wp) = '1.0.0'
  expect_false(v1 == version(wp))
  expect_equal(version(wp), '1.0.0')
})