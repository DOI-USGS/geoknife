context("using query for webdata")


test_that("webdata query returns a datagroup", {
  datasets <<- query('webdata')
  expect_is(datasets, 'datagroup')
})

test_that("webdata query returns a list", {
  wd <- webdata(datasets[4])
  expect_is(query(wd,'variables'), 'character')
})
