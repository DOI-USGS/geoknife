

setClass(
	Class = "geoknife",
	representation = representation(
		WFS_URL="character",WPS_URL="character",
		algorithm="list",
		processInputs="list",feature="list",processID="character",
		WPS_DEFAULT_VERSION="character",WFS_DEFAULT_VERSION="character",
		WPS_DEFAULT_NAMESPACE="character",OWS_DEFAULT_NAMESPACE="character",
		WPS_SCHEMA_LOCATION="character",XSI_SCHEMA_LOCATION="character",
		GML_SCHEMA_LOCATION="character",DRAW_SCHEMA_LOCATION="character",
		WFS_NAMESPACE="character",OGC_NAMESPACE="character",
		GML_NAMESPACE="character",DRAW_NAMESPACE="character",
		SMPL_NAMESPACE="character",UPLD_NAMESPACE="character",
		CSW_NAMESPACE="character",XLINK_NAMESPACE="character",
		XSI_NAMESPACE="character",UTILITY_URL="character",
		UPLOAD_URL="character",algorithms="list",
		upload="character",dataList="character",
		timeList="character",emailK="character")
		)

#' Create geoknife object 
#'
#'@export
#'@rdname geoknife
#'@import XML
#'@import RCurl
#'@keywords geoknife
#'@examples 
#'\dontrun{
#'gk <- geoknife() # create geoknife object
#'gk # print geoknife object
#'
#'linearRing = bufferPoint(c(-111.48,36.95))
#'setFeature(gk) <-list(LinearRing=linearRing)
#'
#' #get a list of available processing algorithms
#'getAlgorithms(gk)
#'
#' #set processing algorithm to feature weighted grid statistics (unweighted will likely fail, because the ring won't intersect the centroids)
#'setAlgorithm(gk) <- getAlgorithms(gk)[4] # feature weighted
#'
#' # set the post inputs for the processing dataset
#'setProcessInputs(gk) <- list('DATASET_ID'='Downward_longwave_radiation_flux_surface',
#'                                        'DATASET_URI'='dods://igsarm-cida-thredds1.er.usgs.gov:8081/qa/thredds/dodsC/nldas/best',
#'                                        'TIME_START'='2010-01-01T00:00:00Z',
#'                                        'TIME_END'='2010-01-01T23:00:00Z',
#'                                        'DELIMITER'='TAB')
#'gk # print geoknife object contents
#'
#' # kick off your request
#'gk <- startProcess(gk)
#'status.gk  <-  checkProcess(gk)
#'checkProcess(gk) # check again and print to screen
#'}
geoknife = function(){
	geoknife = new("geoknife")
	return(geoknife)
}

setMethod(f="initialize",signature="geoknife",
	definition=function(.Object){
		default_WFS = 'http://cida.usgs.gov/gdp/geoserver/wfs'
		default_WPS = 'http://cida.usgs.gov/gdp/process/WebProcessingService'
		default_alg = list()
		default_post = list(empty=NULL)
		default_feat= list(
			FEATURE_COLLECTION=NULL,
			ATTRIBUTE=NULL,
			GML=NA,
			LinearRing=NA)
		# class properties: **PRIVATE**
		.Object@WPS_DEFAULT_VERSION = '1.0.0'
		.Object@WFS_DEFAULT_VERSION = '1.1.0'
		.Object@WPS_DEFAULT_NAMESPACE='http://www.opengis.net/wps/1.0.0'
		.Object@OWS_DEFAULT_NAMESPACE='http://www.opengis.net/ows/1.1'
		# *schema definitions
		.Object@WPS_SCHEMA_LOCATION = 'http://schemas.opengis.net/wps/1.0.0/wpsExecute_request.xsd'
		.Object@XSI_SCHEMA_LOCATION = 'http://www.opengis.net/wfs ../wfs/1.1.0/WFS.xsd'
		.Object@GML_SCHEMA_LOCATION = 'http://schemas.opengis.net/gml/3.1.1/base/feature.xsd'
		.Object@DRAW_SCHEMA_LOCATION = 'http://cida.usgs.gov/climate/derivative/xsd/draw.xsd'
		# *namesspace definitions
		.Object@WFS_NAMESPACE   = 'http://www.opengis.net/wfs'
		.Object@OGC_NAMESPACE   = 'http://www.opengis.net/ogc'
		.Object@GML_NAMESPACE   = 'http://www.opengis.net/gml'
		.Object@DRAW_NAMESPACE  = 'gov.usgs.cida.gdp.draw'
		.Object@SMPL_NAMESPACE  = 'gov.usgs.cida.gdp.sample'
		.Object@UPLD_NAMESPACE  = 'gov.usgs.cida.gdp.upload'
		.Object@CSW_NAMESPACE   = 'http://www.opengis.net/cat/csw/2.0.2'
		.Object@XLINK_NAMESPACE = 'http://www.w3.org/1999/xlink'
		.Object@XSI_NAMESPACE   = 'http://www.w3.org/2001/XMLSchema-instance'

		.Object@UTILITY_URL = 'http://cida.usgs.gov/gdp/utility/WebProcessingService'
		.Object@UPLOAD_URL  = 'http://cida.usgs.gov/gdp/geoserver/'

		# *list of utilities available to this module
		.Object@upload      = 'gov.usgs.cida.gdp.wps.algorithm.filemanagement.ReceiveFiles'
		.Object@dataList    = 'gov.usgs.cida.gdp.wps.algorithm.discovery.ListOpendapGrids'
		.Object@timeList    = 'gov.usgs.cida.gdp.wps.algorithm.discovery.GetGridTimeRange'
		.Object@emailK      = 'gov.usgs.cida.gdp.wps.algorithm.communication.EmailWhenFinishedAlgorithm'

		# public variables (available via print method)	
		.Object@WFS_URL	<-	default_WFS
		.Object@WPS_URL <- default_WPS
		.Object@algorithm	<-	default_alg
		.Object@processInputs	<-	default_post
		.Object@feature	<-	default_feat
		.Object@processID	<-	'<no active job>'
		
		
		return(.Object)
	})


setProcessID	<-	function(.Object,processID){
	.Object@processID	<-	processID
	return(.Object)
}



generateRequest	<-	function(.Object, algorithm,cachedResponse='false'){

		top    <-	newXMLNode(name='wps:Execute',attrs=c('service'="WPS",'version'=.Object@WPS_DEFAULT_VERSION,
			'xsi:schemaLocation'=paste(c(.Object@WPS_DEFAULT_NAMESPACE,.Object@WPS_SCHEMA_LOCATION),collapse=" ")),
			namespaceDefinitions=c('wps'=.Object@WPS_DEFAULT_NAMESPACE,'ows'=.Object@OWS_DEFAULT_NAMESPACE,
			'xlink'=.Object@XLINK_NAMESPACE,'xsi'=.Object@XSI_NAMESPACE))
			
		id	<-	newXMLNode("ows:Identifier",newXMLTextNode(algorithm),parent=top) #algorithm gov.usgs.cida.gdp.wps.algorithm.discovery.ListOpendapGrids
		di	<-	newXMLNode("wps:DataInputs",parent=top)
		addChildren(top,c(id,di))
		
		wi	<-	newXMLNode("wps:Input",parent=di)
		addChildren(di,c(wi))
		
		oi	<-	newXMLNode("ows:Identifier",newXMLTextNode('catalog-url'),parent=wi)
		wd	<-	newXMLNode("wps:Data",parent=wi)
		addChildren(wi,c(oi,wd))
		
		wld	<-	newXMLNode("wps:LiteralData",newXMLTextNode(.Object@processInputs$DATASET_URI),parent=wd)
		addChildren(wd,wld)
		
		wi	<-	newXMLNode("wps:Input",parent=di)
		addChildren(di,wi)
		
		owi	<-	newXMLNode("ows:Identifier",newXMLTextNode('allow-cached-response'),parent=wi)
		wpd	<-	newXMLNode("wps:Data",parent=wi)
		addChildren(wi,c(owi,wpd))
		
		if (!cachedResponse){
			allowCachedResponse	<-	'false'
		} else {
			allowCachedResponse	<-	'true'
		}
		
		wpLd<-	newXMLNode("wps:LiteralData",newXMLTextNode(allowCachedResponse),parent=wpd)
		addChildren(wpd,wpLd)
		
		wrf	<-	newXMLNode("wps:ResponseForm",parent=top)
		wrd	<-	newXMLNode("wps:ResponseDocument",parent=wrf)
		wpo	<-	newXMLNode("wps:Output",parent=wrd)
		owi	<-	newXMLNode("ows:Identifier",newXMLTextNode("result"),parent=wpo)
		addChildren(top,wrf)
		requestXML	<-	toString.XMLNode(xmlDoc(top))

		return(requestXML)
}

genericExecute	<-	function(url,requestXML){
	myheader	<-	c(Connection="close", 
	          			'Content-Type' = "application/xml")
	
	data	<-	 getURL(url = url,
	               postfields=requestXML,
	               httpheader=myheader,
	               verbose=FALSE)		
	xmltext 	<-	xmlTreeParse(data, asText = TRUE,useInternalNodes=TRUE)
	response	<-	xmlRoot(xmltext)
	return(response)

	#response	<-	xmlRoot(xmltext)
	#responseNS	<-	xmlNamespaceDefinitions(response, simplify = TRUE)  
	#processID	<-	xmlGetAttr(response,"statusLocation")
	
	#.Object	<-	setProcessID(.Object,processID)
	#return(.Object)
}

setList	<-	function(ObjectField,varList){
	if (!is.list(varList)){
		stop("field data must be a list")
	}
	if (length(varList)>0){
		# else, empty list passed to be set
		vNames	<-	names(varList)
		for (n in 1:length(vNames)){
			if (!any(names(ObjectField)==vNames[n])){
				stop(paste(vNames[n],"is an invalid field name",sep=' '))
			}
			ObjectField[vNames[n]]	<-	varList[n]
		}
	}
	
	return(ObjectField)
}

parseXMLnodes	<-	function(xmlURL,parentKey,childKey,key="name"){
	doc	<-	htmlParse(xmlURL,isURL=TRUE, useInternalNodes = TRUE)
	if (is.na(key)){
		nodes	<-	getNodeSet(doc,paste(c("//",parentKey,"/",childKey),collapse=""))
	} else {
		nodes	<-	getNodeSet(doc,paste(c("//",parentKey,"/",childKey,"/",key),collapse=""))
	}
	values	<-	sapply(nodes,xmlValue)
	return(values)
}
parseXMLattributes	<-	function(xmlURL,parentKey,childKey,key="name"){
	doc	<-	htmlParse(xmlURL,isURL=TRUE, useInternalNodes = TRUE)
	nodes	<-	getNodeSet(doc,paste(c("//",parentKey,"[@",childKey,"]"),collapse=""))
	# will error if none found
	values	<-	list()
	for (i in 1:length(nodes)){
		values[[i]]	<-	xmlGetAttr(nodes[[i]],key)
	}
	values	<-	unlist(values[values != "the_geom" & values != ""])
	return(values)
}
parseXMLvalues	<-	function(xmlURL,key){
	doc	<-	htmlParse(xmlURL,isURL=TRUE, useInternalNodes = TRUE)
	nodes	<-	getNodeSet(doc,paste(c("//",tolower(key)),collapse=""))
	# will error if none found
	values	<-	sapply(nodes,xmlValue)
	return(values)
}

#'printProcessInputs
#'
#'function for printing processXML \code{geoknife} object. 
#'
#'@param An \code{geoknife} object.
#'@keywords printProcessInputs
#'@export
#'@examples gk <- geoknife() # create geoknife object
#'linearRing = bufferPoint(c(-111.48,36.95))
#'setFeature(gk) <- list(LinearRing=linearRing)
#'
#'algorithm <- list("Area Grid Statistics (weighted)"=
#'"gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
#'setAlgorithm(gk) <- algorithm
#'setProcessInputs(gk) <- list("DATASET_URI" = "dods://igsarm-cida-thredds1.er.usgs.gov:8081/qa/thredds/dodsC/nldas/best")
#'printProcessInputs(gk)
printProcessInputs	<-	function(.Object){
	# FAILS when process inputs are NULL...
	requestXML	<-	suppressWarnings(processInputsToXML(.Object))
	print(requestXML)
	
	# needs a valid algorithm
}








