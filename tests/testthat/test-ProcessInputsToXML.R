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

test_that("Execute XML creation with two points works", {
  wd <- readRDS("data/test_XML_two_points_wd.rds") # wd <- webdata('prism'); times(wd)[2] <- times(wd)[1]
  wg <- readRDS("data/test_XML_two_points_wg.rds") # wg <-simplegeom(data.frame(point1 = c(-48.6, 45.2), point2=c(-88.6, 45.2)))
  wp <- readRDS("data/test_XML_two_points_wp.rds") # wp <- webprocess() 
  xml <- XML(wg, wd, wp)
  fn <- "data/test_XML_two_points_xml.xml"
  expect_equal(xml,readChar(fn, file.info(fn)$size))
})