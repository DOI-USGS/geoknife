context("using query for webdata")


test_that("webdata query returns a datagroup", {
  datasets <<- query('webdata')
  expect_is(datasets, 'datagroup')
})

test_that("webdata query returns a list", {
  wd <- webdata(datasets[4])
  expect_is(query(wd,'variables'), 'character')
})

test_that("webdata title returns a title", {
  expect_is(title(datasets[4]), 'character')
})

test_that("webdata url returns a url", {
  expect_is(url(datasets[4]), 'character')
  expect_is(url(datasets[4:5]), 'character')
})


test_that("webdata abstract returns an abstract", {
  expect_is(url(datasets[4]), 'character')
})