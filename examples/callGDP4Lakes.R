library("rGDP")

# ---- variables -----
WPS	<-	'http://cida-wiwsc-gdp1qa.er.usgs.gov:8080/gdp-process-wps/WebProcessingService'
WFS <-  'https://www.sciencebase.gov/catalogMaps/mapping/ows/5064a227e4b0050306263069'
feature_collection	<-	"sb:mendota"
attribute	<-	'ComID'
value <-  '143249470'
datasetURI  <-	'dods://cida-eros-thredds1.er.usgs.gov:8081/qa/thredds/dodsC/temp/Simard_Pinto_3DGlobalVeg_JGR.tif'
var	<-	'I0B0'
# ---- variables -----

rGDP	<-	rGDP()
rGDP	<-	setAlgorithm(rGDP,getAlgorithms(rGDP)[4])

print(rGDP)

rGDP	<-	setWFS(rGDP,WFS)
#rGDP@WFS_DEFAULT_VERSION = '1.0.0'
rGDP  <-	setWPS(rGDP,WPS)

getShapefiles(rGDP)
getAttributes(rGDP,feature_collection)
getValues(rGDP,feature_collection,attribute)
rGDP	<-	setFeature(rGDP,list('FEATURE_COLLECTION'=feature_collection,
	'ATTRIBUTE'=attribute))
rGDP	<-	setPostInputs(rGDP,list('DATASET_ID'=var,
								'DATASET_URI'=datasetURI))

print(rGDP)

rGDP	<-	executePost(rGDP)

print(rGDP)