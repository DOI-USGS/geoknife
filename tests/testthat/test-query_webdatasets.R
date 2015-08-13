context("using query for webdata")


test_that("webdata query returns a list", {
  expect_warning(datasets <<- query('webdata'))
  expect_is(datasets, 'list')
})

test_that("webdata query returns a list", {
  wd <- webdata(url=datasets[[4]])
  expect_is(query(wd,'variables'), 'character')
})
