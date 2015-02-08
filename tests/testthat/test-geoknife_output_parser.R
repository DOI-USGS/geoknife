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
  expect_equal(output$Alabama[output$variable == 'Prcp' & output$statistic == 'MEAN(mm/d)'][6], 25.76490800)
  expect_equal(output$Connecticut[output$variable == 'Prcp' & output$statistic == 'MEAN(mm/d)'][6], 12.5118475)
  expect_equal(output$Alabama[output$variable == 'Tmin' & output$statistic == 'MEAN(C)'][1], 7.776923)
  expect_equal(output$Idaho[output$variable == 'Wind' & output$statistic == 'VARIANCE(m/s^2)'][2], 1.8166552)
  expect_equal(output$DateTime[output$variable == 'Wind' & output$statistic == 'VARIANCE(m/s^2)'][2], as.POSIXct('1950-01-02 00:00', tz = 'UTC'))
})

test_that("timeseries parser works on complex output csv", {
  local_file <- system.file('extdata','csv_multi_feature_stat.csv', package = 'geoknife')
  output <- parseTimeseries(local_file, delim = ',')
  expect_is(output, "data.frame")
  expect_error(parseTimeseries(local_file, delim = ' '))
})

test_that("timeseries parser works on multi var multi feature", {
  local_file <- system.file('extdata','tsv_multi_feature_var.tsv', package = 'geoknife')
  output <- parseTimeseries(local_file, delim = '\t')
  expect_is(output, "data.frame")
  expect_error(parseTimeseries(local_file, delim = ' '))
})


