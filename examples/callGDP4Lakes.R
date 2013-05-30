
library("rGDP")

# ---- variables -----
WFS	<-	'https://www.sciencebase.gov/catalogMaps/mapping/ows/5064a227e4b0050306263069'
WPS	<-	'http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/gdp-process-wps/WebProcessingService'
feature_collection	<-	'sb:mendota_shape'
attribute	<-	'ComID'
datasetURI	<-	'dods://igsarm-cida-thredds1.er.usgs.gov:8081/qa/thredds/dodsC/nldas/best'
var	<-	'Downward_shortwave_radiation_flux_surface'
# ---- variables -----

rGDP	<-	rGDP()

rGDP	<-	setWFS(rGDP,WFS)
rGDP	<-	setWPS(rGDP,WPS)
rGDP	<-	setFeature(rGDP,list('FEATURE_COLLECTION'=feature_collection,
	'ATTRIBUTE'=attribute))
rGDP	<-	setPostInputs(rGDP,list('DATASET_ID'=var,
                                'TIME_START'="2000-01-01T00:00:00Z",
                                'TIME_END'="2000-03-01T00:00:00Z"))
rGDP	<-	setDatasetURI(rGDP,datasetURI)

print(rGDP)

rGDP	<-	executePost(rGDP)

print(rGDP)