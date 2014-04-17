library("geoknife")

# ---- variables -----
WFS <-  'https://www.sciencebase.gov/catalogMaps/mapping/ows/51b0f5e5e4b030b51983cda1'
feature_collection  <-	'sb:WBIC_190900'
attribute  <-	'WBDY_WBIC'
datasetURI	<-	'http://ags.cr.usgs.gov/ArcGIS/services/NED_1/MapServer/WCSServer'
var	<-	'1'
# ---- variables -----

# instantiate geoknife object	
geoknife	<-	geoknife()
# the first available algorithm is a get WCS subset, which returns a geotiff
setAlgorithm(geoknife) <- getAlgorithms(geoknife)[1]

# show what 'cha got
print(geoknife)

# set the web feature service
geoknife	<-	setWFS(geoknife,WFS)

# set the web processing service
WPS <- 'http://cida-eros-gdp2qa.er.usgs.gov/gdp/process/WebProcessingService'
geoknife <- setWPS(geoknife,WPS)

# set the feature collection for the element in the service that you want to use
geoknife	<-	setFeature(geoknife,list('FEATURE_COLLECTION'=feature_collection,
	'ATTRIBUTE'=attribute))
	
# set the process inputs that you want to use
geoknife	<-	setProcessInputs(geoknife,list('DATASET_ID'=var,
								'DATASET_URI'=datasetURI))

# show what 'cha got
print(geoknife)

# execute what you have
geoknife	<-	startProcess(geoknife)

status.geoknife  <-  checkProcess(geoknife)

cat('checking status of GDP request. Large complex requests take longer to process.\n')
repeat{
	if (!is.null(status.geoknife$URL) | status.geoknife$status!=""){
	    break
	  }
  cat('checking process...\n')
  Sys.sleep(10)
  if (is.null(status.geoknife$URL)){
    status.geoknife  <-  checkProcess(geoknife)
  }
}

if (status.geoknife$status=='Process successful'){
	cat(paste(status.geoknife$status,'\nDownload available at: ',status.geoknife$URL,sep=''))
} else {
	cat(status.geoknife$status)
}
