context("show webdata")

test_that("show webdata", {
  webdata()
})

context("show webgeom")

test_that("show webgeom", {
  webgeom()
})

context("show simplegeom")

test_that("show simplegeom", {
  simplegeom(c(-88.6, 45.2))
  expect_error(simplegeom(c(-88.6, 45.2,99)))
})

context("show datagroup")

test_that("show datagroup", {
  query('webdata')
})

context("show geojob")

test_that("show geojob", {
  geojob()
})

context("show webprocess")

test_that("show webprocess", {
  webprocess()
})