# Run this script in the testthat directory to refresh all the rds files loaded by tests.
# Try to use RDS files when static objects can be used to keep chattyness of tests down.

library(geoknife)

if(!"geoknife/tests/testthat" == substr(getwd(), nchar(getwd())-22, nchar(getwd()))) {
  warning("this script assumes it is in the testthat directory of the geoknife package!!!")
}

gconfig(wps.url = "https://cida-test.er.usgs.gov/gdp/process/WebProcessingService")
fabric <- webdata('prism')
times(fabric)[2] <- "1895-01-01"
saveRDS(fabric, "data/test_webdata_fabric.rds")
wp <- webprocess()
saveRDS(wp, "data/test_webprocess_knife.rds")

default.sleep <- geoknife:::gconfig('sleep.time')
saveRDS(webprocess(sleep.time = default.sleep+5), "data/test_webprocess_sleep-plus-five.rds")
saveRDS(query(wp, 'algorithms'), "data/test_webprocess_algorithms.rds")

saveRDS(webprocess(DELIMITER = 'TAB'), "data/test_webprocess_tab.rds")

gconfig('wps.url'="https://cida.usgs.gov/gdp/process/WebProcessingService")
wp <- webprocess()
saveRDS(wp, "data/test_webprocess_knife_prod.rds")
saveRDS(query(wp, 'algorithms'), "data/test_webprocess_algorithms_prod.rds")

gconfig(wps.url = "https://cida-test.er.usgs.gov/gdp/process/WebProcessingService")

saveRDS(webgeom('HUC8::09020306'), "data/test_webgeom_huc08.rds")
saveRDS(webgeom('state::Wisconsin'), "data/test_webgeom_WI.rds")
saveRDS(simplegeom(data.frame(point1 = c(-48.6, 45.2), point2=c(-88.6, 45.2))),
        "data/test_simplegeom_two_points.rds")

saveRDS(webdata('prism',times = as.POSIXct(c('2001-01-01','2002-02-05'))),
        "data/test_webdata_prism_year.rds")

# from custom data source vignette
wg <- webgeom(url="https://www.sciencebase.gov/catalogMaps/mapping/ows/54296bf0e4b0ad29004c2fbb")
geom(wg) <- "sb:Yahara_River_HRUs_alb_eq"
attribute(wg) <- "GRIDCODE"

wd <- webdata(url = 'https://cida.usgs.gov/thredds/dodsC/prism_v2', 
                  variables = c('tmx', 'tmn', 'ppt'),
                  times = as.POSIXct(c('2000-01-01', '2010-01-01')))

saveRDS(wg, "data/test_XML_no_gmlid_wg.rds")
saveRDS(wd, "data/test_XML_no_gmlid_wd.rds")

wp <- webprocess(algorithm = list('OPeNDAP Subset'="gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm"))
inputs(wp, "OUTPUT_TYPE") <- "geotiff"
saveRDS(wp, "data/test_XML_wp_opendapsubset.rds")

webdata <- query("webdata")
saveRDS(webdata, "data/test_query_webdata.rds")
