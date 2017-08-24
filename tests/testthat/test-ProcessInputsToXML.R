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
  expect_equal(xml,readChar(fn, file.info(fn)$size))
}) 
