# library("XML")
# library("RCurl")
# 

#' rGDP package 
#'
#' \tabular{ll}{
#' Package: \tab rGDP\cr
#' Type: \tab Package\cr
#' Version: \tab 0.0.1\cr
#' Date: \tab 2013-05-23\cr
#' License: \tab Unlimited for this package, dependencies have more restrictive licensing.\cr
#' Copyright: \tab This software is in the public domain because it contains materials
#' that originally came from the United States Geological Survey, an agency of
#' the United States Department of Interior. For more information, see the
#' official USGS copyright policy at
#' http://www.usgs.gov/visual-id/credit_usgs.html#copyright\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Collection of functions to do GDP calls
#'
#' @name rGDP-package
#' @docType package
#' @import XML RCurl
#' @author Jordan Read \email{jread@@usgs.gov}
NULL


#' GDP class
#'
#' Some details about the \code{GDP} class
#'
#' @name GDP-class
#' @rdname GDP-class
#' @exportClass GDP
setClass(
	Class = "GDP",
	representation = representation(
		WFS_URL="character",PROCESS_URL="character",
		datasetURI="character",algorithm="character",
		PostInputs="list",feature="list",processID="character",
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


# 'Initialize Methods for \code{GDP} objects
# '
# 'Initialize the \code{GDP} object....
# '
# '@name initialize
# '@aliases initialize
# '@docType methods
# '@keywords initialize
# '@exportMethod initialize
setMethod(f="initialize",signature="GDP",
	definition=function(.Object){
		default_WFS = 'http://cida-eros-gdp2.er.usgs.gov:8082/geoserver/wfs'
		default_WPS = 'http://cida.usgs.gov/gdp/process/WebProcessingService'
		default_URI = 'dods://cida.usgs.gov/thredds/dodsC/prism'
		default_alg = 'FWGS'
		default_feat= list(
			FEATURE_COLLECTION=NA,
			ATTRIBUTE=NA,
			GML=NA)
		# class properties: **PRIVATE**
		.Object@WPS_DEFAULT_VERSION = '1.0.0'
		.Object@WFS_DEFAULT_VERSION = '1.1.0'
		.Object@WPS_DEFAULT_NAMESPACE='http://www.opengis.net/wps/1.0.0'
		.Object@OWS_DEFAULT_NAMESPACE='http://www.opengis.net/ows/1.1'
		# *schema definitions
		.Object@WPS_SCHEMA_LOCATION = 'http://schemas.opengis.net/wps/1.0.0/wpsExecute_request.xsd'
		.Object@XSI_SCHEMA_LOCATION = 'http://www.opengis.net/wfs ../wfs/1.1.0/WFS.xsd'
		.Object@GML_SCHEMA_LOCATION = 'http://schemas.opengis.net/gml/3.1.1/base/feature.xsd'
		.Object@DRAW_SCHEMA_LOCATION = 'http://cida.usgs.gov/qa/climate/derivative/xsd/draw.xsd'
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

		.Object@algorithms = list(
			FWGS='gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm',
			FCOD='gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm',
			FCI='gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageIntersectionAlgorithm',
			FCGC='gov.usgs.cida.gdp.wps.algorithm.FeatureCategoricalGridCoverageAlgorithm')

		# *list of utilities available to this module
		.Object@upload      = 'gov.usgs.cida.gdp.wps.algorithm.filemanagement.ReceiveFiles'
		.Object@dataList    = 'gov.usgs.cida.gdp.wps.algorithm.discovery.ListOpendapGrids'
		.Object@timeList    = 'gov.usgs.cida.gdp.wps.algorithm.discovery.GetGridTimeRange'
		.Object@emailK      = 'gov.usgs.cida.gdp.wps.algorithm.communication.EmailWhenFinishedAlgorithm'

		# public variables (available via print method)	
		.Object@WFS_URL	<-	default_WFS
		.Object@PROCESS_URL <- default_WPS
		.Object@datasetURI	<-	default_URI
		.Object@algorithm	<-	default_alg
		.Object	<-	initializePostInputs(.Object)
		.Object@feature	<-	default_feat
		.Object@processID	<-	"Null"
		
		
		return(.Object)
	})
# move this//?
setGeneric(name="initializePostInputs",def=function(.Object){standardGeneric("initializePostInputs")})
setGeneric(name="setProcessID",def=function(.Object,processID){standardGeneric("setProcessID")})
setGeneric(name="getShapefiles",def=function(.Object){standardGeneric("getShapefiles")})
setGeneric(name="getAttributes",def=function(.Object,shapefile){standardGeneric("getAttributes")})
setGeneric(name="getValues",def=function(.Object,shapefile,attribute){standardGeneric("getValues")})
setGeneric(name="checkProcess",def=function(.Object){standardGeneric("checkProcess")})
setGeneric(name="executePost",def=function(.Object){standardGeneric("executePost")})
setGeneric(name="setWFS",def=function(.Object,wfs){standardGeneric("setWFS")})
setGeneric(name="setDatasetURI",def=function(.Object,datasetURI){standardGeneric("setDatasetURI")})
setGeneric(name="setWPS",def=function(.Object,wps){standardGeneric("setWPS")})
setGeneric(name="setPostInputs",def=function(.Object,postInputs){standardGeneric("setPostInputs")})
setGeneric(name="setFeature",def=function(.Object,feature){standardGeneric("setFeature")})
setGeneric(name="setAlgorithm",def=function(.Object,algorithm){standardGeneric("setAlgorithm")})

setMethod(f = "initializePostInputs",signature="GDP",
	definition =	function(.Object){
		algorithm	<-	.Object@algorithm
		if (algorithm=="FWGS"){
			.Object@PostInputs	<-	list("FEATURE_ATTRIBUTE_NAME"=NA,
				"DATASET_URI"=.Object@datasetURI,
				"DATASET_ID"=NA,"TIME_START"=NA,
				"TIME_END"=NA,"REQUIRE_FULL_COVERAGE"="true",
				"DELIMITER"="TAB","STATISTICS"="MEAN",
				"GROUP_BY"="STATISTIC","SUMMARIZE_TIMESTEP"="false",
				"SUMMARIZE_FEATURE_ATTRIBUTE"="false")
		}
		return(.Object)
		
	})

setMethod(f = "setProcessID",signature="GDP",
	definition = function(.Object,processID){
		.Object@processID	<-	processID
		return(.Object)
	})
	
setMethod(f = "getShapefiles",signature="GDP",
	definition = function(.Object){
		parentKey 	<-	"featuretypelist"
		childKey	<-	"featuretype"
		processURL	<-	paste(c(.Object@WFS_URL,'?service=WFS&version=',
			.Object@WFS_DEFAULT_VERSION,'&request=GetCapabilities'),collapse="")
		
		shapefiles	<-	parseXMLnodes(processURL,parentKey,childKey)
		return(shapefiles)
	})
	
setMethod(f = "getAttributes",signature="GDP",
	definition = function(.Object,shapefile=.Object@feature$FEATURECOLLECTION){
		parentKey	<-	"element"
		childKey	<-	"maxoccurs"
		processURL	<-	paste(c(.Object@WFS_URL,'?service=WFS&version=',
			.Object@WFS_DEFAULT_VERSION,'&request=DescribeFeatureType',
			'&typename=',shapefile),collapse="")
		attributes	<-	parseXMLattributes(processURL,parentKey,childKey)
		return(attributes)
	})
setMethod(f = "getValues",signature="GDP",
	definition = function(.Object,shapefile=.Object@feature$FEATURE_COLLECTION,
		attribute=.Object@feature$ATTRIBUTE){
		processURL	<-	paste(c(.Object@WFS_URL,'?service=WFS&version=',
			.Object@WFS_DEFAULT_VERSION,'&request=GetFeature',
			'&info_format=text%2Fxml&typename=',shapefile,
			'&propertyname=',attribute),collapse="")
		values	<-	parseXMLvalues(processURL,attribute)
		return(values)
	})

setMethod(f = "checkProcess",signature="GDP",
	definition = function(.Object){
		fileURL	<-	 NA
		status	<-	"none"
		return(list(fileURL=fileURL,status=status))	
	})	

setMethod(f = "executePost",signature="GDP",
	definition = function(.Object){
		return(.Object)
	})	

setMethod(f = "setWFS",signature="GDP",
	definition = function(.Object,wfs){
		.Object@WFS_URL	<-	wfs
		return(.Object)
	})

setMethod(f = "setWPS",signature="GDP",
	definition = function(.Object,wps){
		.Object@PROCESS_URL	<-	wps
		return(.Object)
	})

setMethod(f = "setDatasetURI",signature = "GDP",
	definition = function(.Object,datasetURI){
		.Object@datasetURI	<-	datasetURI
		.Object	<-	setPostInputs(.Object,list(DATASET_URI=.Object@datasetURI))
		return(.Object)
	})
	
setMethod(f = "setPostInputs",signature = "GDP",
	definition = function(.Object,postInputs){
		.Object@PostInputs	<-	setList(.Object@PostInputs,postInputs)
		return(.Object)
	})
	
setMethod(f = "setFeature",signature = "GDP",
	definition = function(.Object,feature){
		.Object@feature	<-	setList(.Object@feature,feature)
		.Object@PostInputs	<-	setList(.Object@PostInputs,
			list("FEATURE_ATTRIBUTE_NAME"=.Object@feature$ATTRIBUTE))
		return(.Object)
	})
	
setMethod(f = "setAlgorithm",signature = "GDP",
	definition = function(.Object,algorithm){
		.Object@algorithm	<-	algorithm
		return(.Object)
	})

setList	<-	function(ObjectField,varList){
	if (!is.list(varList)){stop("field data must be a list")}
	vNames	<-	names(varList)
	for (n in 1:length(vNames)){
		if (!any(names(ObjectField)==vNames[n])){stop("invalid field name")}
		ObjectField[vNames[n]]	<-	varList[n]
	}
	return(ObjectField)
	}

parseXMLnodes	<-	function(xmlURL,parentKey,childKey,key="name"){
	doc	<-	htmlParse(xmlURL,isURL=TRUE, useInternalNodes = TRUE)
	nodes	<-	getNodeSet(doc,paste(c("//",parentKey,"/",childKey,"/",key),collapse=""))
	values	<-	sapply(nodes,xmlValue)
	return(values)
}
parseXMLattributes	<-	function(xmlURL,parentKey,childKey,key="name"){
	doc	<-	htmlParse(xmlURL,isURL=TRUE, useInternalNodes = TRUE)
	nodes	<-	getNodeSet(doc,paste(c("//",parentKey,"[@",childKey,"]"),collapse=""))
	# will error if none found
	for (i in 1:length(nodes)){
		values[[i]]	<-	xmlGetAttr(nodes[[i]],key)
	}
	values	<-	values[values != "the_geom" & values != ""]
	return(values)
}
parseXMLvalues	<-	function(xmlURL,key){
	doc	<-	htmlParse(xmlURL,isURL=TRUE, useInternalNodes = TRUE)
	nodes	<-	getNodeSet(doc,paste(c("//",tolower(key)),collapse=""))
	# will error if none found
	values	<-	sapply(nodes,xmlValue)
	return(values)
}

postInputsToXML	<-	function(.Object){
	top    <-	newXMLNode(name='wps:Execute',attrs=c('service'="WPS",'version'=.Object@WPS_DEFAULT_VERSION,
		'xsi:schemaLocation'=paste(c(.Object@WPS_DEFAULT_NAMESPACE,.Object@WPS_SCHEMA_LOCATION),collapse=" ")),
		namespaceDefinitions=c('wps'=.Object@WPS_DEFAULT_NAMESPACE,'ows'=.Object@OWS_DEFAULT_NAMESPACE,
		'xlink'=.Object@XLINK_NAMESPACE,'xsi'=.Object@XSI_NAMESPACE))
	
	
	id	<-	newXMLNode("ows:Identifier",newXMLTextNode(.Object@algorithms[.Object@algorithm]),parent=top)
	di	<-	newXMLNode("wps:DataInputs",parent=top)
	addChildren(top,c(id,di))
	
	for (i in 1:length(.Object@PostInputs)){
		postNm	<-	names(.Object@PostInputs[i])
		postVl	<-	.Object@PostInputs[postNm]
		inEL	<-	newXMLNode("wps:Input",parent=di)
		addChildren(di,inEL)
		
		inIdEL   <- newXMLNode("ows:Identifier",newXMLTextNode(postNm),parent=inEL)
		addChildren(inEL,inIdEL)
		
		inDatEL  <- newXMLNode("wps:Data")
		addChildren(inEL,inDatEL);
		
		litDatEL	<-	newXMLNode('wps:LiteralData',newXMLTextNode(postVl))
		addChildren(inDatEL,litDatEL)
	}
	# complex data
	inEL	<-	newXMLNode("wps:Input")
	addChildren(di,inEL)
	inIdEL   <- newXMLNode('ows:Identifier',newXMLTextNode('FEATURE_COLLECTION'))
	addChildren(inEL,inIdEL)
	inDatEL  <- newXMLNode('wps:Reference',attrs=c("xlink:href"=.Object@WFS_URL))
	addChildren(inEL,inDatEL)
	
	bodyEL   <-	newXMLNode('wps:Body')
	addChildren(inDatEL,bodyEL)
	
	featEL   <-	newXMLNode('wfs:GetFeature',attrs=c("service"="WFS",
		"version"=.Object@WFS_DEFAULT_VERSION,
		"outputFormat"="text/xml; subtype=gml/3.1.1",
		"xsi:schemaLocation"=.Object@XSI_SCHEMA_LOCATION),
		namespaceDefinitions=c("wfs"=.Object@WFS_NAMESPACE,
		"ogc"=.Object@OGC_NAMESPACE,
		"gml"=.Object@GML_NAMESPACE,
		"xsi"=.Object@XSI_NAMESPACE))
	addChildren(bodyEL,featEL)
	queryEL  <-	newXMLNode('wfs:Query',attrs=c("typeName"=as.character(.Object@feature['FEATURE_COLLECTION'])))
	addChildren(featEL,queryEL)
	propNmEL <-	newXMLNode('wfs:PropertyName',newXMLTextNode('the_geom'))
	addChildren(queryEL,propNmEL)
	propNmEL<-	newXMLNode('wfs:PropertyName',newXMLTextNode(.Object@feature['ATTRIBUTE']))
	addChildren(queryEL,propNmEL)
	if (any(names(.Object@feature)=='GML') & !is.na(.Object@feature['GML'])){
		filterEL	<-	newXMLNode('ogc:Filter')
		addChildren(queryEL,filterEL)
		gmlObEL	<-	newXMLNode('ogc:GmlObjectId',attrs=c('gml:id'=.Object@feature['GML']))
		addChildren(filterEL,gmlObEL)
	}	
	resForm	<-	newXMLNode('wps:ResponseForm')
	addChildren(top,resForm)
	
	resDoc	<-	newXMLNode('wps:ResponseDocument',attrs=c('storeExecuteResponse'='true','status'='true'))
	addChildren(resForm,resDoc)
	
	resOut	<-	newXMLNode('wps:Output',attrs=c('asReference'='true'))
	addChildren(resDoc,resOut)
	
	outID	<-	newXMLNode('ows:Identifier',newXMLTextNode('OUTPUT'))
	addChildren(resOut,outID)
	
	requestXML <-toString.XMLNode(xmlDoc(top))
	
	return(requestXML)
}

setMethod(f = "executePost",signature = "GDP",definition = function(.Object){
	
	requestXML	<-	postInputsToXML(.Object)
	myheader=c(Connection="close", 
	           'Content-Type' = "application/xml")#text/xml?

	data =  getURL(url = .Object@PROCESS_URL,
	               postfields=requestXML, #requestXML,
	               httpheader=myheader,
	               verbose=TRUE)
				
	xmltext  <- xmlTreeParse(data, asText = TRUE,useInternalNodes=TRUE)
	response <- xmlRoot(xmltext)
	responseNS <- xmlNamespaceDefinitions(response, simplify = TRUE)  
	processID <- xmlGetAttr(response,"statusLocation")
	
	.Object	<-	setProcessID(.Object,processID)
	return(.Object)
})

setMethod(f = "checkProcess",signature = "GDP",definition = function(.Object){
	
	if (.Object@processID=="Null"){
		status	<-	'none'
	}
	else{
		
	}
	checkForComplete	<-	getURL(url = .Object@processID, verbose=TRUE)
	checkForCompleteResponse	<-	xmlTreeParse(checkForComplete, asText = TRUE,useInternalNodes=TRUE)
	checkResponseNS <- xmlNamespaceDefinitions(checkForCompleteResponse, simplify = TRUE) 
	root <- xmlRoot(checkForCompleteResponse)
	status <- sapply(xmlChildren(root[["Status"]]), xmlName)
	cat(status, "\n")

	if ("ProcessSucceeded" == status){
		root <- xmlRoot(checkForCompleteResponse)
	    gdpURL <- as.character(xpathApply(root, "//@href", namespaces = checkResponseNS)[[1]])
	    gdpID <- strsplit(gdpURL, "id=")[[1]][2]
	    break
	}
})

setProcessID	<-	function(.Object,processID){
	.Object@processID	<-	processID
	return(.Object)
}

# create summary method......

setMethod(f = "print",signature = "GDP",
	function(x,...){
		cat("*** Class GDP, method Print *** \n")
		cat("* WFS_URL:\t");cat(x@WFS_URL,"\n")
		cat("* PROCESS_URL:\t");cat(x@PROCESS_URL,"\n")
		cat("* datasetURI:\t");cat(x@datasetURI,"\n")
		cat("* algorithm:\t");cat(x@algorithm,"\n")
		cat("* ------PostInputs------\n")
		Li	<-	unlist(x@PostInputs)
		for (i in 1:length(Li)){cat("\t-",names(Li[i]));cat(":",Li[i],"\n")}
		cat("* ------feature------\n")
		Li	<-	unlist(x@feature)
		for (i in 1:length(Li)){cat("\t-",names(Li[i]));cat(":",Li[i],"\n")}
		cat("* processID:\t");cat(x@processID,"\n")
		cat("**** End Print (GDP)**** \n")
	}
)
setMethod(f = "show",signature = "GDP",definition = function(object){print(object)})
		
GDP = function(){
	rGDP = new("GDP")
	return(rGDP)
}
