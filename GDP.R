


	
# class properties: **PRIVATE** can be set by methods
setClass(
	Class = "GDP",
	representation = representation(
		WFS_URL="character",PROCESS_URL="character",
		datasetURI="character",algorithm="character",
		PostInputs="list",feature="list",processID="character"))

setMethod(f="initialize",signature="GDP",
	definition=function(.Object,PROCESS_URL="asdf"){
		# class properties: **PRIVATE**
		WPS_DEFAULT_VERSION = '1.0.0'
		WFS_DEFAULT_VERSION = '1.1.0'
		WPS_DEFAULT_NAMESPACE='http://www.opengis.net/wps/1.0.0'
		OWS_DEFAULT_NAMESPACE='http://www.opengis.net/ows/1.1'
		# *schema definitions
		WPS_SCHEMA_LOCATION = 'http://schemas.opengis.net/wps/1.0.0/wpsExecute_request.xsd'
		XSI_SCHEMA_LOCATION = 'http://www.opengis.net/wfs ../wfs/1.1.0/WFS.xsd'
		GML_SCHEMA_LOCATION = 'http://schemas.opengis.net/gml/3.1.1/base/feature.xsd'
		DRAW_SCHEMA_LOCATION = 'http://cida.usgs.gov/qa/climate/derivative/xsd/draw.xsd'
		# *namesspace definitions
		WFS_NAMESPACE   = 'http://www.opengis.net/wfs'
		OGC_NAMESPACE   = 'http://www.opengis.net/ogc'
		GML_NAMESPACE   = 'http://www.opengis.net/gml'
		DRAW_NAMESPACE  = 'gov.usgs.cida.gdp.draw'
		SMPL_NAMESPACE  = 'gov.usgs.cida.gdp.sample'
		UPLD_NAMESPACE  = 'gov.usgs.cida.gdp.upload'
		CSW_NAMESPACE   = 'http://www.opengis.net/cat/csw/2.0.2'
		XLINK_NAMESPACE = 'http://www.w3.org/1999/xlink'
		XSI_NAMESPACE   = 'http://www.w3.org/2001/XMLSchema-instance'

		UTILITY_URL = 'http://cida.usgs.gov/gdp/utility/WebProcessingService'
		UPLOAD_URL  = 'http://cida.usgs.gov/gdp/geoserver/'

		algorithms = list(
			FWGS='gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm',
			FCOD='gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm',
			FCI='gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageIntersectionAlgorithm',
			FCGC='gov.usgs.cida.gdp.wps.algorithm.FeatureCategoricalGridCoverageAlgorithm')

		# *list of utilities available to this module
		upload      = 'gov.usgs.cida.gdp.wps.algorithm.filemanagement.ReceiveFiles'
		dataList    = 'gov.usgs.cida.gdp.wps.algorithm.discovery.ListOpendapGrids'
		timeList    = 'gov.usgs.cida.gdp.wps.algorithm.discovery.GetGridTimeRange'
		emailK      = 'gov.usgs.cida.gdp.wps.algorithm.communication.EmailWhenFinishedAlgorithm'

		default_WFS = 'http://cida-eros-gdp2.er.usgs.gov:8082/geoserver/wfs'
		default_WPS = 'http://cida.usgs.gov/gdp/process/WebProcessingService'
		default_URI = 'dods://cida.usgs.gov/qa/thredds/dodsC/prism'
		default_alg = 'FWGS'
		default_feat= list(
			FEATURE_COLLECTION=NA,
			ATTRIBUTE=NA,
			GLM=NA)
			
		.Object@WFS_URL	<-	default_WFS
		.Object@PROCESS_URL <- default_WPS
		.Object@datasetURI	<-	default_URI
		.Object@algorithm	<-	default_alg
		.Object	<-	initializePostInputs(.Object)
		.Object@feature	<-	default_feat
		.Object@processID	<-	character()
		return(.Object)
	})

setGeneric(name="initializePostInputs",def=function(.Object){standardGeneric("initializePostInputs")})

setMethod(f = "initializePostInputs",signature="GDP",
	definition =	function(.Object){
		algorithm	<-	.Object@algorithm
		if (algorithm=="FWGS"){
			.Object@PostInputs	<-	list("FEATURE_ATTRIBUTE_NAME"=NA,"DATASET_URI"=NA)
		}
		return(.Object)
		
	})
setMethod(f="print","GDP",
	function(x,...){
		cat("*** Class GDP, method Print *** \n")
		cat("* WFS_URL:\t");print(x@WFS_URL)
		cat("* PROCESS_URL:\t");print(x@PROCESS_URL)
		cat("* datasetURI:\t");print(x@PROCESS_URL)
		cat("* algorithm:\t");print(x@algorithm)
		Li	<-	unlist(x@PostInputs)
		for (i in 1:length(Li)){cat("* PostInputs-");cat(names(Li[i]));cat(":",Li[i],"\n")}
		Li	<-	unlist(x@feature)
		for (i in 1:length(Li)){cat("* feature-");cat(names(Li[i]));cat(":",Li[i],"\n")}
		cat("* processID:\t");print(x@processID)
		cat("**** End Print (GDP)**** \n")
	}
)
#getMethod(f="getAttributes",signature="GDP",
#	function(x,...){print('asdfa')})		
GDP = function(){
	rGDP = new("GDP")
	return(rGDP)
}