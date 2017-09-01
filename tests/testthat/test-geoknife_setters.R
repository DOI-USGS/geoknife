context("Test setting of geoknife algorithms and process inputs")


test_that("webprocess can set algorithms", {
  testthat::skip_on_cran()
  wp <- webprocess()
  algs <- query(wp, 'algorithms')
  expect_error(algorithm(wp)<-'bad.char')
  expect_error(algorithm(wp)<-NULL)
  expect_error(algorithm(wp)<-list('junk'='will break process'))
  
  algorithm(wp) <- algs[1]
  # test that it properly sets
  expect_equal(algorithm(wp), algs[1])
  
})

context("Test setting of webgeom simple sets")
test_that("webgeom can set states", {
  testthat::skip_on_cran()
  states <- c("Colorado","Oregon","Wisconsin")
  wg <- webgeom(paste0('state::',paste(states,collapse = ',')))
  expect_equal(length(values(wg)), length(states))
})

context("geoknife sets stencil correctly")
test_that("geoknife sets stencil correctly", {
  testthat::skip_on_cran()
  job <- geoknife('HUC8::09020306', 'prism')
  expect_is(job, 'geojob')
  cancel(job)
  job <- geoknife(data.frame('point1'=c(-89, 46), 'point2'=c(-88.6, 45.2)), 'prism')
  expect_is(job, 'geojob')
  cancel(job)
})

library(sp)
Sr1 <- Polygon(cbind(c(-89.0001,-89,-88.9999,-89,-89.0001),c(46,46.0001,46,45.9999,46)))
Sr2 <- Polygon(cbind(c(-88.6,-88.5999,-88.5999,-88.6,-88.6),c(45.2,45.2,45.1999,45.1999,45.2)))
Srs1 <- Polygons(list(Sr1), "s1")
Srs2 <- Polygons(list(Sr2), "s2")
test_that("geoknife converts SpatialPolygons to simplegeoms correctly", {
  
  SP <- SpatialPolygons(list(Srs1,Srs2), proj4string = CRS("+proj=longlat +datum=WGS84"))
  expect_is(simplegeom(SP), 'simplegeom')
  expect_error(simplegeom(SpatialPolygons(list(Srs1,Srs2))))
})

context("geoknife w/ knife modified in line")
test_that("geoknife sets knife correctly", {
  testthat::skip_on_cran()
  job <- geoknife('HUC8::09020306', 'prism', wait=TRUE)
  expect_true(geoknife:::canStart())
  cancel(job)
})

context("setting webgeom values")
test_that("NULL results in error", {
  testthat::skip_on_cran()
  wg_c <- webgeom(geom = 'derivative:US_Counties', attribute = 'COUNTY')
  expect_silent(values(wg_c) <- "Belmont County")
  expect_error(values(wg_c) <- "foo")
})

test_that("we can set attributes on a webgeom", {
  testthat::skip_on_cran()
  stencil <- webgeom()
  geom(stencil) <- "sample:CONUS_states"
  attribute(stencil) <- "STATE"
  expect_equal(stencil@attribute, "STATE")
  stencil_attribute <- attribute(stencil)
  expect_equal(stencil_attribute, "STATE")
})

context("basics of geoknife processing job are as expected")

test_that("algorithm version works", {
  fabric <- webdata('prism')
  times(fabric)[2] <- '1895-01-01'
  job <- geoknife(stencil = c(-89,42), fabric = fabric)
  expect_equal(job@algorithm.version, "1.0.0")
  cancel(job)
})

test_that("version method works", {
  stencil <- webgeom()
  version(stencil) <- "2.0.0"
  expect_equal(version(stencil), "2.0.0")
})
