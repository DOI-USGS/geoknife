library("rGDP")
library("RCurl")
# ---- variables -----


WPS  <-	'http://cida.usgs.gov/qa/climate/gdp/process/WebProcessingService'
WPS <- 'http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/gdp-process-wps/WebProcessingService'
WFS <-  'https://www.sciencebase.gov/catalogMaps/mapping/ows/51b1ef75e4b022a6a540fa5c'
attribute  <-  'WBDY_WBIC'

lake = list()

# -- create rGDP objects to collect metadata --
rGDP  <-	rGDP()
rGDP  <-  setWPS(rGDP,WPS)
rGDP	<-	setAlgorithm(rGDP,getAlgorithms(rGDP)[4]) # feature weighted grid
rGDP  <-	setWFS(rGDP,WFS)
feature_collection <- getShapefiles(rGDP)[2] # [1] is footprint, [2] is WBIC_###etc
rGDP  <-	setFeature(rGDP,list('FEATURE_COLLECTION'=feature_collection,
      'ATTRIBUTE'=attribute))

GDP_elv <-  setPostInputs(rGDP,list('DATASET_ID'='1',
                                'DATASET_URI'='http://ags.cr.usgs.gov/ArcGIS/services/NED_1/MapServer/WCSServer'))
GDP_elv  <-	executePost(GDP_elv)


GDP_canopy <-  setPostInputs(rGDP,list('DATASET_ID'='I0B0',
                                    'DATASET_URI'='dods://cida-eros-thredds1.er.usgs.gov:8081/qa/thredds/dodsC/temp/Simard_Pinto_3DGlobalVeg_JGR.tif'))
GDP_canopy  <-  executePost(GDP_canopy)

GDP_driver  <-  setPostInputs(rGDP,list('DATASET_ID'='Downward_shortwave_radiation_flux_surface',
                                        'DATASET_URI'='dods://igsarm-cida-thredds1.er.usgs.gov:8081/qa/thredds/dodsC/nldas/best',
                                        'TIME_START'='1979-01-02T00:00:00Z',
                                        'TIME_END'='1979-01-31T23:00:00Z',
                                        'DELIMITER'='TAB'))

GDP_driver  <-  executePost(GDP_driver)
GDP_name  <-  GDP_canopy
GDP_name@WFS_DEFAULT_VERSION = '1.0.0'

lake$name  <-  getValues(GDP_name,feature_collection,'WBDY_NAME')
lake$area  <-  getValues(GDP_name,feature_collection,'Shape_Area')
lake$wbic  <-  getValues(GDP_name,feature_collection,'WBDY_WBIC')

status_elv  <-  checkProcess(GDP_elv)
status_canopy <-  checkProcess(GDP_canopy)
status_driver <-  checkProcess(GDP_driver)
ptm <- proc.time()
repeat{
  if (!is.null(status_elv$URL) & !is.null(status_canopy$URL) & !is.null(status_driver$URL)){
    lake$elev = unlist(strsplit(unlist(strsplit(getURL(status_elv$URL),','))[4],'\n'))[1]
    lake$canopy = unlist(strsplit(unlist(strsplit(getURL(status_canopy$URL),','))[4],'\n'))[1]
    lake$driver = status_driver$URL
    break
  }
  cat('checking process...\n')
  Sys.sleep(10)
  if (is.null(status_elv$URL)){
    status_elv  <-  checkProcess(GDP_elv)
  }
  if (is.null(status_canopy$URL)){
    status_canopy <-  checkProcess(GDP_canopy)
  }
  status_driver <-  checkProcess(GDP_driver)
}
proc.time() - ptm
lake

