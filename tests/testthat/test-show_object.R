context("show object methods")

test_that("show webdata", {
  expect_output(show(readRDS("data/test_webdata_fabric.rds")))
})

test_that("show webgeom", {
  expect_output(show(readRDS("data/test_webgeom_WI.rds")))
})

test_that("show simplegeom", {
  expect_output(show(readRDS("data/test_simplegeom_two_points.rds")))
  expect_error(simplegeom(c(-88.6, 45.2,99)))
})

test_that("show datagroup", {
  testthat::skip_on_cran()
  expect_output(show(readRDS("data/test_query_webdata.rds")))
})

test_that("show geojob", {
  expect_output(show(readRDS("data/test_email_gj.rds")))
})

test_that("show webprocess", {
  testthat::skip_on_cran()
  expect_output(show(readRDS("data/test_webprocess_knife.rds")))
})