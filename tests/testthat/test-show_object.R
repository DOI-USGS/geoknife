context("show webdata")

test_that("show webdata", {
  expect_output(show(webdata()))
})

context("show webgeom")

test_that("show webgeom", {
  expect_output(show(webgeom()))
})

context("show simplegeom")

test_that("show simplegeom", {
  expect_output(show(simplegeom(c(-88.6, 45.2))))
  expect_error(simplegeom(c(-88.6, 45.2,99)))
})

context("show datagroup")

test_that("show datagroup", {
  expect_output(show(query('webdata')))
})

context("show geojob")

test_that("show geojob", {
  expect_output(show(geojob()))
})

context("show webprocess")

test_that("show webprocess", {
  expect_output(show(webprocess()))
})