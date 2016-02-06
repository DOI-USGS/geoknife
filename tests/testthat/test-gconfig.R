context("gconfig")


test_that("can get read.only variables", {
  expect_equal(gconfig(no.readonly = FALSE), gconfig())
  expect_false(length(gconfig(no.readonly = TRUE)) == length(gconfig()))
})

test_that("can't set read.only variables", {
  expect_error(gconfig('version'='2.0.0'))
})

test_that("can set !read.only variables", {
  expect_false(gconfig('wait'))
  gconfig('wait'=TRUE)
  expect_true(gconfig('wait'))
})