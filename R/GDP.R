
#' rGDP class
#'
#' Some details about the \code{rGDP} class
#' \describe{
#'		\item{WFS_URL}{endpoint for web feature service (WFS)}
#'		\item{WPS_URL}{endpoint for web processing service (WPS)}
#'		\item{algorithm}{a length one list (name and location) of a WPS algorithm}
#'		\item{processInputs}{a list of process parameters}
#'		\item{feature}{a list of elements in the feature collection}
#`		\item{processID}{unique identifier for a GDP process}
#' }
#'
#' @name rGDP
#' @rdname rGDP-class
#' @export
setClass(
	Class = "rGDP",
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

#' @export
# '@docType functions
# '@rdname rGDP
# '@keywords rGDP
rGDP = function(){
	rGDP = new("rGDP")
	return(rGDP)
}

setMethod(f="initialize",signature="rGDP",
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



#'getAlgorithms
#'
#'a \code{rGDP} method for finding algorithm names and locations from a valid WPS endpoint.
#'
#'@param \code{rGDP} object with a valid WPS url.
#'@return list of available algorithms for the \code{rGDP} WPS url.
#'@docType methods
#'@keywords getAlgorithms
#'@export
setGeneric(name="getAlgorithms",def=function(.Object){standardGeneric("getAlgorithms")})
#'getShapefiles
#'
#'a \code{rGDP} method for finding shapefile names at a valid WFS endpoint.
#'
#'@param \code{rGDP} object with a valid WFS url.
#'@return list of shapefiles for the \code{rGDP} WFS url.
#'@docType methods
#'@keywords getShapefiles
#'@export
setGeneric(name="getShapefiles",def=function(.Object){standardGeneric("getShapefiles")})
#'getAttributes
#'
#'a \code{rGDP} method for finding attribute names for a given shapefile at a valid WFS endpoint. 
#'
#'@param \code{rGDP} object with a valid WFS url.
#'@param a valid shapefile name.
#'@return list of attributes for the given shapefile at the \code{rGDP} WFS url.
#'@docType methods
#'@keywords getAttributes
#'@export
setGeneric(name="getAttributes",def=function(.Object,shapefile){standardGeneric("getAttributes")})
#'getValues
#'
#'a \code{rGDP} method for finding value names for a given shapefile at a valid WFS endpoint. 
#'
#'@param \code{rGDP} object with a valid WFS url.
#'@param a valid shapefile name.
#'@param a valid attribute name for the shapefile.
#'@return list of values for the given shapefile attribute at the \code{rGDP} WFS url.
#'@docType methods
#'@keywords getValues
#'@export
setGeneric(name="getValues",def=function(.Object,shapefile,attribute){standardGeneric("getValues")})

#'setWFS
#'
#'method for setting the web feature service (WFS) endpoint for a \code{rGDP} object. 
#'
#'@param \code{rGDP} object.
#'@param a Web Feature Service (WFS) endpoint.
#'@return An \code{rGDP} object.
#'@docType methods
#'@keywords setWFS
#'@export
setGeneric(name="setWFS",def=function(.Object,wfs){standardGeneric("setWFS")})
#'setWPS
#'
#'method for setting the web processing service (WPS) endpoint for a \code{rGDP} object. 
#'
#'@param \code{rGDP} object.
#'@param a Web Processing Service (WPS) endpoint.
#'@return An \code{rGDP} object.
#'@docType methods
#'@keywords setWPS
#'@export
setGeneric(name="setWPS",def=function(.Object,wps){standardGeneric("setWPS")})
#'setFeature
#'
#'method for setting the feature elements of the \code{rGDP} object. 
#'
#'@param An \code{rGDP} object.
#'@param a list containing a valid feature collection, or a list of a subset of a valid feature collection.
#'@return An \code{rGDP} object.
#'@docType methods
#'@keywords setFeature
#'@export
setGeneric(name="setFeature",def=function(.Object,feature){standardGeneric("setFeature")})
#'setAlgorithm
#'
#'method for setting the process algorithm of the \code{rGDP} object.
#'
#'@param An \code{rGDP} object.
#'@param a list for a valid algorithm, including values for name & location
#'@return An \code{rGDP} object.
#'@docType methods
#'@keywords setAlgorithm
#'@export
setGeneric(name="setAlgorithm",def=function(.Object,algorithm){standardGeneric("setAlgorithm")})


setGeneric(name="setProcessID",def=function(.Object,processID){standardGeneric("setProcessID")})

setProcessID	<-	function(.Object,processID){
	.Object@processID	<-	processID
	return(.Object)
}

# '@rdname getAlgorithms-methods
# '@aliases getAlgorithms,rGDP-method	
setMethod(f = "getAlgorithms",signature="rGDP",
	definition = function(.Object){
		processURL	<-	paste(c(.Object@WPS_URL,'?service=WPS&version=',
			.Object@WPS_DEFAULT_VERSION,'&request=GetCapabilities'),collapse="")
		algorithm.Loc	<-	parseXMLnodes(processURL,"process","identifier",key=NA)
		algorithm.Nm	<-	parseXMLnodes(processURL,"process","title",key=NA)
		algorithms	<-	list()
		for (i in 1:length(algorithm.Nm)){
			algorithms[[i]]	<-	algorithm.Loc[i]
		}
		names(algorithms)	<-	algorithm.Nm
		return(algorithms)
	})
# '@rdname getShapefiles-methods
# '@aliases getShapefiles,rGDP-method	
setMethod(f = "getShapefiles",signature="rGDP",
	definition = function(.Object){
		parentKey 	<-	"featuretypelist"
		childKey	<-	"featuretype"
		processURL	<-	paste(c(.Object@WFS_URL,'?service=WFS&version=',
			.Object@WFS_DEFAULT_VERSION,'&request=GetCapabilities'),collapse="")
		shapefiles	<-	parseXMLnodes(processURL,parentKey,childKey)
		return(shapefiles)
	})
# '@rdname getAttributes-methods
# '@aliases getAttributes,rGDP-method	
setMethod(f = "getAttributes",signature="rGDP",
	definition = function(.Object,shapefile){
		parentKey	<-	"element"
		childKey	<-	"maxoccurs"
		processURL	<-	paste(c(.Object@WFS_URL,'?service=WFS&version=',
			.Object@WFS_DEFAULT_VERSION,'&request=DescribeFeatureType',
			'&typename=',shapefile),collapse="")
		attributes	<-	parseXMLattributes(processURL,parentKey,childKey)
		return(attributes)
	})
# '@rdname getValues-methods
# '@aliases getValues,rGDP-method
setMethod(f = "getValues",signature="rGDP",
	definition = function(.Object,shapefile,
		attribute){
		processURL	<-	paste(c(.Object@WFS_URL,'?service=WFS&version=',
			.Object@WFS_DEFAULT_VERSION,'&request=GetFeature',
			'&info_format=text%2Fxml&typename=',shapefile,
			'&propertyname=',attribute),collapse="")
		values	<-	parseXMLvalues(processURL,attribute)
		return(values)
	})



# '@rdname setWFS-methods
# '@aliases setWFS,rGDP-method
setMethod(f = "setWFS",signature="rGDP",
	definition = function(.Object,wfs){
		wfs	<-	gsub('https', 'http', wfs)
		.Object@WFS_URL	<-	wfs
		return(.Object)
	})
# '@rdname setWPS-methods
# '@aliases setWPS,rGDP-method
setMethod(f = "setWPS",signature="rGDP",
	definition = function(.Object,wps){
		wps	<-	gsub('https', 'http', wps)
		.Object@WPS_URL	<-	wps
		return(.Object)
	})

# '@rdname setFeature-methods
# '@aliases setFeature,rGDP-method	
setMethod(f = "setFeature",signature = "rGDP",
	definition = function(.Object,feature){
		
		if ("LinearRing" %in% names(feature)){
			# if we are setting the LinearRing, all other feature elements should be wiped
			if (length(names(feature)) > 1){
				stop('Cannot set LinearRing and WFS components for single feature')
			} else {
				sw.idx	<-	names(.Object@feature)!='LinearRing'
				hid.feature	<-	.Object@feature[sw.idx]
				hid.feature[]	<-	'hidden'
				# set all other elements to 'hidden'
				.Object@feature	<-	setList(.Object@feature,hid.feature)
				.Object@feature	<-	setList(.Object@feature,feature)

			}
		} else {
			hid.feature	<-	list(LinearRing='hidden')
			.Object@feature	<-	setList(.Object@feature,feature)
			.Object@feature	<-	setList(.Object@feature,hid.feature)
			if ("FEATURE_ATTRIBUTE_NAME" %in% names(.Object@processInputs)){
			.Object@processInputs	<-	setList(.Object@processInputs,
				list("FEATURE_ATTRIBUTE_NAME"=.Object@feature$ATTRIBUTE))}
		}
	
		
		return(.Object)
	})
# '@rdname setAlgorithm-methods
# '@aliases setAlgorithm,rGDP-method	
setMethod(f = "setAlgorithm",signature = "rGDP",
	definition = function(.Object,algorithm){
		.Object@algorithm	<-	algorithm
		# now, initialize posts
		.Object	<-	initializeProcessInputs(.Object)
		return(.Object)
	})

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
#'function for printing postXML \code{rGDP} object. 
#'
#'@param An \code{rGDP} object.
#'@keywords printProcessInputs
#'@export
printProcessInputs	<-	function(.Object){
	requestXML	<-	suppressWarnings(processInputsToXML(.Object))
	print(requestXML)
	
	# needs a valid algorithm
}








