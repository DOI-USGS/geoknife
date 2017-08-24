context("test email xml templating")

test_that("basic email template works", {
  wp <- readRDS("data/test_email_wp.rds")
  gj <- readRDS("data/test_email_gj.rds")
  xml <- geoknife:::make_email_execute_xml(gj, wp)
  
  fn <- "data/test_email.xml"
  expect_equal(xml,readChar(fn, file.info(fn)$size))
})