context("Test xml parsing")

test_that("parseXMLnodes works for WFS responses", {
  local_file <- system.file('extdata','SB_getCapabilities.xml', package = 'geoknife')
  xml <- XML::xmlParseDoc(local_file)
  output <- geoknife:::parseXMLgeoms(xml)
  expect_is(output, "character")
  expect_equal(output[2], "sb:WBIC_190900")
})