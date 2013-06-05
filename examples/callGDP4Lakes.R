library("rGDP")

# ---- variables -----
WPS	<-	'http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/gdp-process-wps/WebProcessingService'
WFS <-  'https://www.sciencebase.gov/catalogMaps/mapping/ows/5064a227e4b0050306263069'
feature_collection	<-	"sb:mendota"
attribute	<-	'ComID'
datasetURI	<-	'dods://cida.usgs.gov/thredds/dodsC/prism'
var	<-	'ppt'
# ---- variables -----

rGDP	<-	rGDP()
rGDP	<-	setAlgorithm(rGDP,getAlgorithms(rGDP)[4])

print(rGDP)

rGDP	<-	setWFS(rGDP,WFS)
rGDP  <-	setWPS(rGDP,WPS)

getShapefiles(rGDP)
getAttributes(rGDP,feature_collection)
rGDP	<-	setFeature(rGDP,list('FEATURE_COLLECTION'=feature_collection,
	'ATTRIBUTE'=attribute))
rGDP	<-	setPostInputs(rGDP,list('DATASET_ID'=var,
                                'TIME_START'="2000-01-01T00:00:00Z",
                                'TIME_END'="2000-03-01T00:00:00Z",
								'DATASET_URI'=datasetURI))

print(rGDP)

rGDP	<-	executePost(rGDP)

print(rGDP)