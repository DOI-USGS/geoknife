context("dodsReplace")


test_that("prism is replaced", {
  expect_equal(geoknife:::dodsReplace('https://cida.usgs.gov/thredds/dodsC/prism_v2'),'dods://cida.usgs.gov/thredds/dodsC/prism_v2')
})

#URL currently down
#test_that("nldas is replaced", {
#  expect_equal(geoknife:::dodsReplace('https://hydro1.sci.gsfc.nasa.gov/dods/NLDAS_FORA0125_M.002'),'dods://hydro1.sci.gsfc.nasa.gov/dods/NLDAS_FORA0125_M.002')
#})