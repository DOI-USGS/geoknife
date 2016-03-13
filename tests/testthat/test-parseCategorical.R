context("parse categorical")

test_that("internal parsing works", {
  local.file <- system.file('extdata','csv_categorical_multifeature.csv', package = 'geoknife')
  output <- parseCategorical(local.file, delim = ',')
  expect_equal(output[['Driftless Area']][output$category=='Sample Count' & output$variable == 'housing_classes_iclus_a1_2010'], 4735427)
  expect_equal(output[['Wyoming Basin']][output$category=='3' & output$variable == 'housing_classes_iclus_a1_2100'], 0.0006708306)
  expect_is(output, 'data.frame')
})

test_that('result works for categorical', {
  testthat::skip_on_cran()
  cancel()
  knife <- webprocess(algorithm = list('Categorical Coverage Fraction'="gov.usgs.cida.gdp.wps.algorithm.FeatureCategoricalGridCoverageAlgorithm"), 
                      wait=TRUE)
  
  library(sp)
  Sr1 = Polygon(cbind(c(-89,-89.2,-89.3,-89.2,-89),c(42,42.1,42,41.9,42)))
  
  Srs1 = Polygons(list(Sr1), "sample.poly")
  stencil <- simplegeom(Srl = list(Srs1), proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  job <- geoknife(stencil, 'iclus', knife)
  expect_is(result(job), 'data.frame')
})