context("Test file parsing")

test_that("timeseries parser works on multi feature, single variable", {
  local_file <- system.file('extdata','tsv_multi_feature.tsv', package = 'geoknife')
  output <- parseTimeseries(local_file, delim = '\t')
  expect_is(output, "data.frame")
})

test_that("timeseries parser works on linear ring, single variable", {
  local_file <- system.file('extdata','tsv_linear_ring.tsv', package = 'geoknife')
  output <- parseTimeseries(local_file, delim = '\t')
  expect_is(output, "data.frame")
})

test_that("timeseries parser works on linear ring, comma delimited", {
  local_file <- system.file('extdata','csv_linear_ring.csv', package = 'geoknife')
  output <- parseTimeseries(local_file, delim = ',')
  expect_is(output, "data.frame")
  expect_error(parseTimeseries(local_file, delim = '\t'))
})

test_that("timeseries parser works on complex output tsv", {
  local_file <- system.file('extdata','tsv_multi_var_feature_stat.tsv', package = 'geoknife')
  output <- parseTimeseries(local_file, delim = '\t')
  expect_is(output, "data.frame")
  expect_error(parseTimeseries(local_file, delim = ','))
})

test_that("timeseries parser works on complex output csv", {
  local_file <- system.file('extdata','csv_multi_feature_stat.csv', package = 'geoknife')
  output <- parseTimeseries(local_file, delim = ',')
  expect_is(output, "data.frame")
  expect_error(parseTimeseries(local_file, delim = ' '))
})



