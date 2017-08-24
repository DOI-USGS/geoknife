context("test XML method")

test_that("basic XML creation works", {
  wd <- readRDS("data/test_XML_wd.rds") # webdata('prism',times = as.POSIXct(c('2001-01-01','2002-02-05')))
  wg <- readRDS("data/test_XML_wg.rds") # webgeom('state::Wisconsin')
  wp <- readRDS("data/test_XML_wp.rds") # webprocess()
  xml <- XML(wg, wd, wp)
  fn <-"data/test_XML_wg_xml.xml"
  expect_equal(xml,readChar(fn, file.info(fn)$size))
  sg <- simplegeom(c(-89,45))
  xml <- XML(sg, wd, webprocess())
  fn <-"data/test_XML_sg_xml.xml"
  expect_equal(nchar(xml),nchar(readChar(fn, file.info(fn)$size)))
}) 

test_that("Execute XML creation with two points works", {
  wd <- readRDS("data/test_XML_two_points_wd.rds") # wd <- webdata('prism'); times(wd)[2] <- times(wd)[1]
  wg <- readRDS("data/test_XML_two_points_wg.rds") # wg <-simplegeom(data.frame(point1 = c(-48.6, 45.2), point2=c(-88.6, 45.2)))
  wp <- readRDS("data/test_XML_two_points_wp.rds") # wp <- webprocess() 
  xml <- XML(wg, wd, wp)
  fn <- "data/test_XML_two_points_xml.xml"
  expect_equal(xml,readChar(fn, file.info(fn)$size))
})

test_that("Execute XML works with no gmlids and multiple variables", {
  # This examples was grabbed from the custom dataset vignette.
    wd <- readRDS("data/test_XML_no_gmlid_wd.rds") 
    wg <- readRDS("data/test_XML_no_gmlid_wg.rds") 
    wp <- readRDS("data/test_XML_no_gmlid_wp.rds")
    xml <- XML(wg, wd, wp)
    fn <- "data/test_XML_no_gmlid_xml.xml"
    expect_equal(xml,readChar(fn, file.info(fn)$size))
  })

test_that("you can set TAB delimited and get back the right XML", {
  wp <- readRDS("data/test_XML_wp.rds")
  inputs(wp, "DELIMITER") <- "TAB"
  xml <- XML(stencil = readRDS("data/test_XML_wg.rds"),
             fabric = readRDS("data/test_XML_wd.rds"), 
             knife = wp)
  expect_true(grepl('mimeType="text/tab-separated-values"', xml))
})

test_that("you can set output_type geotiff and get back the right XML", {
  # Generated rds with this code.
  # wp <- webprocess(algorithm = list('OPeNDAP Subset'="gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm"))
  # inputs(wp, "OUTPUT_TYPE") <- "geotiff"
  # saveRDS(wp, "data/test_XML_wp_opendapsubset.rds")
  wp <- readRDS("data/test_XML_wp_opendapsubset.rds")
  xml <- XML(stencil = readRDS("data/test_XML_wg.rds"),
             fabric = readRDS("data/test_XML_wd.rds"), 
             knife = wp)
  expect_true(grepl('mimeType="application/zip"', xml))
})
