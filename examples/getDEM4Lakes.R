library("rGDP")

# ---- variables -----
WFS <-  'https://www.sciencebase.gov/catalogMaps/mapping/ows/51b0f5e5e4b030b51983cda1'
feature_collection  <-	'sb:WBIC_190900'
attribute  <-	'WBDY_WBIC'
datasetURI	<-	'http://ags.cr.usgs.gov/ArcGIS/services/NED_1/MapServer/WCSServer'
var	<-	'1'
# ---- variables -----

# instantiate rGDP object	
rGDP	<-	rGDP()
# the first available algorithm is a get WCS subset, which returns a geotiff
rGDP	<-	setAlgorithm(rGDP,getAlgorithms(rGDP)[1])

# show what 'cha got
print(rGDP)

# set the web feature service
rGDP	<-	setWFS(rGDP,WFS)

# set the feature collection for the element in the service that you want to use
rGDP	<-	setFeature(rGDP,list('FEATURE_COLLECTION'=feature_collection,
	'ATTRIBUTE'=attribute))
	
# set the process inputs that you want to use
rGDP	<-	setPostInputs(rGDP,list('DATASET_ID'=var,
								'DATASET_URI'=datasetURI))

# show what 'cha got
print(rGDP)

# execute what you have
rGDP	<-	startProcess(rGDP)

status.rGDP  <-  checkProcess(rGDP)

cat('checking status of GDP request. Large complex requests take longer to process.\n')
repeat{
	if (!is.null(status.rGDP$URL) | status.rGDP$status!=""){
	    break
	  }
  cat('checking process...\n')
  Sys.sleep(10)
  if (is.null(status.rGDP$URL)){
    status.rGDP  <-  checkProcess(rGDP)
  }
}

if (status.rGDP$status=='Process successful'){
	cat(paste(status.rGDP$status,'\nDownload available at: ',status.rGDP$URL,sep=''))
} else {
	cat(status.rGDP$status)
}
