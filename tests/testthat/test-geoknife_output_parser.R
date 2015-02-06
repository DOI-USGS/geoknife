context("Test file parsing")

test_that("timeseries parser works on multi feature, single variable", {
  local_file <- system.file('extdata','tsv_multi_feature.tsv', package = 'geoknife')
  output <- parseTimeseries(local_file, delim = '\t')
  expect_is(output, "list")
})

test_that("timeseries parser works on linear ring, single variable", {
  local_file <- system.file('extdata','tsv_linear_ring.tsv', package = 'geoknife')
  output <- parseTimeseries(local_file, delim = '\t')
  expect_is(output, "list")
})

test_that("timeseries parser works on linear ring, comma delimited", {
  local_file <- system.file('extdata','csv_linear_ring.csv', package = 'geoknife')
  output <- parseTimeseries(local_file, delim = ',')
  expect_is(output, "list")
  expect_error(parseTimeseries(local_file, delim = '\t'))
})

