context("download and load")


test_that("download works", {
  cancel()
  job <<- geoknife(stencil = c(-89,42), fabric = 'prism', wait=TRUE)
  file = download(job)
  expect_true(file.exists(file))
})

test_that("load result works", {
  expect_is(result(job),'data.frame')
})