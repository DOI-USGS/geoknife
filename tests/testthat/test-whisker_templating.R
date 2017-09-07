context("test email xml templating")

test_that("basic email template works", {
  wp <- readRDS("data/test_email_wp.rds")
  gj <- readRDS("data/test_email_gj.rds")
  xml <- geoknife:::make_email_execute_xml(gj, wp)
  
  fn <- "data/test_email.xml"
  expect_equal(xml,readChar(fn, file.info(fn)$size))
})

context("test XML method templating")

test_that("basic XML creation works", {
  wd <- readRDS("data/test_webdata_prism_year.rds")
  wg <- readRDS("data/test_webgeom_WI.rds")
  wp <- readRDS("data/test_webprocess_knife_prod.rds")
  xml <- XML(wg, wd, wp)
  fn <-"data/test_XML_wg_xml.xml"
  expect_equal(xml,readChar(fn, file.info(fn)$size))
  sg <- simplegeom(c(-89,45))
  xml <- XML(sg, wd, wp)
  fn <-"data/test_XML_sg_xml.xml"
  expect_equal(nchar(xml),nchar(readChar(fn, file.info(fn)$size)))
}) 

test_that("Execute XML creation with two points works", {
  wd <- readRDS("data/test_webdata_fabric.rds")
  wg <- readRDS("data/test_simplegeom_two_points.rds")
  wp <- readRDS("data/test_webprocess_knife.rds")
  xml <- XML(wg, wd, wp)
  fn <- "data/test_XML_two_points_xml.xml"
  expect_equal(nchar(xml),nchar(readChar(fn, file.info(fn)$size)))
})

test_that("Execute XML works with no gmlids and multiple variables", {
  # This examples was grabbed from the custom dataset vignette.
  wd <- readRDS("data/test_XML_no_gmlid_wd.rds") 
  wg <- readRDS("data/test_XML_no_gmlid_wg.rds") 
  wp <- readRDS("data/test_webprocess_knife_prod.rds")
  xml <- XML(wg, wd, wp)
  fn <- "data/test_XML_no_gmlid_xml.xml"
  expect_equal(xml,readChar(fn, file.info(fn)$size))
})

test_that("you can set TAB delimited and get back the right XML", {
  wp <- readRDS("data/test_webprocess_knife_prod.rds")
  inputs(wp, "DELIMITER") <- "TAB"
  xml <- XML(stencil = readRDS("data/test_webgeom_WI.rds"),
             fabric = readRDS("data/test_webdata_prism_year.rds"), 
             knife = wp)
  expect_true(grepl('mimeType="text/tab-separated-values"', xml))
})

test_that("you can set output_type geotiff and get back the right XML", {
  wp <- readRDS("data/test_XML_wp_opendapsubset.rds")
  xml <- XML(stencil = readRDS("data/test_webgeom_WI.rds"),
             fabric = readRDS("data/test_webdata_prism_year.rds"), 
             knife = wp)
  expect_true(grepl('mimeType="application/zip"', xml))
})

test_that("execute XML for getgridtimerange works", {
  wd <- readRDS("data/test_webdata_fabric.rds")
  wp <- readRDS("data/test_webprocess_knife_prod.rds")
  xml <- geoknife:::make_listopendapgrids_execute_xml(wd, wp)
  
  fn <- "data/test_listopendapgrids.xml"
  expect_equal(xml,readChar(fn, file.info(fn)$size))
})

test_that("execute XML for getgridtimerange works", {
  wd <- readRDS("data/test_webdata_fabric.rds")
  wp <- readRDS("data/test_webprocess_knife_prod.rds")
  xml <- geoknife:::make_getgridtimerange_execute_xml(wd, wp)
  
  fn <- "data/test_getgridtimerange.xml"
  expect_equal(xml,readChar(fn, file.info(fn)$size))
})