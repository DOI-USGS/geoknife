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
#' rGDP object executes user-specified calls to geo data portal (usgs.cida.gov/gdp) and checks process status. 
#'
#' @name rGDP-package
#' @docType package
#' @import XML RCurl methods
#' @author Jordan Read \email{jread@@usgs.gov}
NULL


#' rGDP class
#'
#' Some details about the \code{rGDP} class
#' \describe{
#'		\item{WFS_URL}{endpoint for web feature service (WFS)}
#'		\item{WPS_URL}{endpoint for web processing service (WPS)}
#'		\item{algorithm}{acronym for WPS algorithm}
#'		\item{postInputs}{a list of process parameters}
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
		postInputs="list",feature="list",processID="character",
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

		# *list of utilities available to this module
		.Object@upload      = 'gov.usgs.cida.gdp.wps.algorithm.filemanagement.ReceiveFiles'
		.Object@dataList    = 'gov.usgs.cida.gdp.wps.algorithm.discovery.ListOpendapGrids'
		.Object@timeList    = 'gov.usgs.cida.gdp.wps.algorithm.discovery.GetGridTimeRange'
		.Object@emailK      = 'gov.usgs.cida.gdp.wps.algorithm.communication.EmailWhenFinishedAlgorithm'

		# public variables (available via print method)	
		.Object@WFS_URL	<-	default_WFS
		.Object@WPS_URL <- default_WPS
		.Object@algorithm	<-	default_alg
		.Object@postInputs	<-	default_post
		.Object@feature	<-	default_feat
		.Object@processID	<-	'<no active job>'
		
		
		return(.Object)
	})
	setMethod(f = "show",signature = "rGDP",definition = function(object){print(object)})

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
#'checkProcess
#'
#'method for checking the process status of an active (executed) \code{rGDP} object. 
#'
#'@param \code{rGDP} object with an active GDP process request.
#'@return process of \code{rGDP} process.
#'@docType methods
#'@keywords checkProcess
#'@export
setGeneric(name="checkProcess",def=function(.Object){standardGeneric("checkProcess")})
#'executePost
#'
#'method for executing the \code{rGDP} object.
#'
#'@param \code{rGDP} object ot be used to formulate GDP process request.
#'@return An \code{rGDP} object.
#'@docType methods
#'@keywords executePost
#'@export
setGeneric(name="executePost",def=function(.Object){standardGeneric("executePost")})
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
#'setPostInputs
#'
#'method for setting the (non-feature related) post inputs of the \code{rGDP} object. 
#'
#'@param An \code{rGDP} object.
#'@param a list of valid postInputs.
#'@return An \code{rGDP} object.
#'@docType methods
#'@keywords setPostInputs
#'@export
setGeneric(name="setPostInputs",def=function(.Object,postInputs){standardGeneric("setPostInputs")})
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

# move this//?
setGeneric(name="initializePostInputs",def=function(.Object){standardGeneric("initializePostInputs")})
setGeneric(name="setProcessID",def=function(.Object,processID){standardGeneric("setProcessID")})

setMethod(f = "initializePostInputs",signature="rGDP",
	definition =	function(.Object){
		algorithm	<-	.Object@algorithm
		processURL	<-	paste(c(.Object@WPS_URL,'?service=WPS&version=',
			.Object@WPS_DEFAULT_VERSION,'&request=DescribeProcess',
			'&identifier=',algorithm),collapse="")
		doc	<-	htmlParse(processURL,isURL=TRUE, useInternalNodes = TRUE)
		optionNd	<-	getNodeSet(doc,'//datainputs/input[@minoccurs=0]/following-sibling::node()[1]')
		optionLs	<-	vector("list",length(optionNd))
		optionLs[]	<-	NA
		names(optionLs)	<-	sapply(optionNd,xmlValue)
		
		requirNd	<-	getNodeSet(doc,'//datainputs/input[@minoccurs>0]/following-sibling::node()[1]')
		requirLs	<-	vector("list",length(requirNd))
		names(requirLs)	<-	sapply(requirNd,xmlValue)
		
		.Object@postInputs	<-	append(optionLs,requirLs)
		.Object	<-	setPostInputs(.Object,requirLs)
		
		# now set any defaults
		defaultNd	<-	getNodeSet(doc,'//datainputs/literaldata/defaultvalue/parent::node()[1]/defaultvalue')
		defaultLs	<-	vector("list",length(sapply(defaultNd,xmlValue)))
		defaultLs[]	<-	sapply(defaultNd,xmlValue)
		names(defaultLs)	<-	sapply(getNodeSet(doc,'//datainputs/literaldata/defaultvalue/
			parent::node()[1]/preceding-sibling::node()[3]'),xmlValue)
			
		if (length(defaultLs)>0){.Object@postInputs	<-	setList(.Object@postInputs,defaultLs)}
		
		# now set any accepted values
		allowNd    <-	getNodeSet(doc,'//datainputs/literaldata//parent::node()/allowedvalues/value[1]')
		allowLs	<-	vector("list",length(sapply(allowNd,xmlValue)))
		allowLs[]	<-	sapply(allowNd,xmlValue)
		names(allowLs)	<-	sapply(getNodeSet(doc,'//datainputs/literaldata/allowedvalues/
			parent::node()[1]/preceding-sibling::node()[3]'),xmlValue)
		if (length(allowLs)>0){.Object@postInputs	<-	setList(.Object@postInputs,allowLs)}
		
		.Object@postInputs$FEATURE_COLLECTION	<-	NULL # handled elsewhere
		return(.Object)
		
	})

setMethod(f = "setProcessID",signature="rGDP",
	definition = function(.Object,processID){
		.Object@processID	<-	processID
		return(.Object)
	})
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

# '@rdname setPostInputs-methods
# '@aliases setPostInputs,rGDP-method	
setMethod(f = "setPostInputs",signature = "rGDP",
	definition = function(.Object,postInputs){
		if (("DATASET_URI" %in% names(postInputs)) & 
			!is.null(postInputs["DATASET_URI"]) & 
			grepl('dodsC',postInputs["DATASET_URI"])){
			postInputs["DATASET_URI"]	<-	gsub('http', 'dods', postInputs["DATASET_URI"])
		}
		.Object@postInputs	<-	setList(.Object@postInputs,postInputs)
		if ("LinearRing" %in% names(.Object@feature) && "FEATURE_ATTRIBUTE_NAME" %in% names(.Object@postInputs)){
			.Object@postInputs$FEATURE_ATTRIBUTE_NAME	<-	'the_geom'
		}
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
			if ("FEATURE_ATTRIBUTE_NAME" %in% names(.Object@postInputs)){
			.Object@postInputs	<-	setList(.Object@postInputs,
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
		.Object	<-	initializePostInputs(.Object)
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
	if (is.na(key)){nodes	<-	getNodeSet(doc,paste(c("//",parentKey,"/",childKey),collapse=""))}
	else{nodes	<-	getNodeSet(doc,paste(c("//",parentKey,"/",childKey,"/",key),collapse=""))}
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
getPostInputs	<-	function(.Object){
	# needs a valid algorithm
}

postInputsToXML	<-	function(.Object){
	top    <-	newXMLNode(name='wps:Execute',attrs=c('service'="WPS",'version'=.Object@WPS_DEFAULT_VERSION,
		'xsi:schemaLocation'=paste(c(.Object@WPS_DEFAULT_NAMESPACE,.Object@WPS_SCHEMA_LOCATION),collapse=" ")),
		namespaceDefinitions=c('wps'=.Object@WPS_DEFAULT_NAMESPACE,'ows'=.Object@OWS_DEFAULT_NAMESPACE,
		'xlink'=.Object@XLINK_NAMESPACE,'xsi'=.Object@XSI_NAMESPACE)) # parameterize this...
	
	
	id	<-	newXMLNode("ows:Identifier",newXMLTextNode(.Object@algorithm),parent=top)
	di	<-	newXMLNode("wps:DataInputs",parent=top)
	addChildren(top,c(id,di))
	
	for (i in 1:length(.Object@postInputs)){
		postNm	<-	names(.Object@postInputs[i])
		postVl	<-	.Object@postInputs[postNm]
		if (!is.na(postVl)){
			inEL	<-	newXMLNode("wps:Input",parent=di)
			addChildren(di,inEL)
		
			inIdEL   <- newXMLNode("ows:Identifier",newXMLTextNode(postNm),parent=inEL)
			addChildren(inEL,inIdEL)
		
			inDatEL  <- newXMLNode("wps:Data")
			addChildren(inEL,inDatEL);
		
			litDatEL	<-	newXMLNode('wps:LiteralData',newXMLTextNode(postVl))
			addChildren(inDatEL,litDatEL)
		}
	}
	# complex data
	inEL	<-	newXMLNode("wps:Input")
	addChildren(di,inEL)
	
	inIdEL   <- newXMLNode('ows:Identifier',newXMLTextNode('FEATURE_COLLECTION'))
	addChildren(inEL,inIdEL)

	# use WFS or LinearRing?
	if (.Object@feature$LinearRing=='hidden' || is.na(.Object@feature$LinearRing)){
		
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
	} else {
		inDatEL	<-	newXMLNode('wps:Data')
		addChildren(inEL,inDatEL)
		
		compDatEL	<-	newXMLNode('wps:ComplexData',attrs=c("mimeType"="text/xml",#,"encoding"="UTF-8",
			"schema"="http://schemas.opengis.net/gml/3.1.1/base/feature.xsd")) # schema needed?
		addChildren(inDatEL,compDatEL)
		
		gmlFeatEL	<-	newXMLNode('gml:featureMembers',namespaceDefinitions=c('gml'="http://www.opengis.net/gml"),
			attrs=c("xsi:schemaLocation"="gov.usgs.cida.gdp.draw http://cida.usgs.gov/qa/climate/derivative/xsd/draw.xsd"))
		addChildren(compDatEL,gmlFeatEL)
		
		gmlBoxEL	<-	newXMLNode('gml:box',attrs=c("gml:id"="box.1"))
		addChildren(gmlFeatEL,gmlBoxEL) # fail
		
		gmlGeomEL	<-	newXMLNode('gml:the_geom') # fail...
		addChildren(gmlBoxEL,gmlGeomEL)
		
		gmlPolyEL	<-	newXMLNode('gml:MultiPolygon',attrs=c("srsDimension"="2","srsName"="http://www.opengis.net/gml/srs/epsg.xml#4326"))
		addChildren(gmlGeomEL,gmlPolyEL)
		
		gmlPmEL	<-	newXMLNode('gml:polygonMember')
		addChildren(gmlPolyEL,gmlPmEL)
		
		gmlPgEL	<-	newXMLNode('gml:Polygon')
		addChildren(gmlPmEL,gmlPgEL)
		
		gmlExEL	<-	newXMLNode('gml:exterior')
		addChildren(gmlPgEL,gmlExEL)
		
		gmlLrEL	<-	newXMLNode('gml:LinearRing')
		addChildren(gmlExEL,gmlLrEL)
		
		gmlPosEL	<-	newXMLNode('gml:posList',newXMLTextNode(paste(.Object@feature[['LinearRing']],collapse=" ")))
		addChildren(gmlLrEL,gmlPosEL)
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

# '@rdname executePost-methods
# '@aliases executePost,rGDP-method
setMethod(f = "executePost",signature = "rGDP",definition = function(.Object){
	
	requestXML	<-	postInputsToXML(.Object)
	myheader=c(Connection="close", 
	          'Content-Type' = "application/xml")#text/xml?
	
	data =  getURL(url = .Object@WPS_URL,
	               postfields=requestXML, #requestXML,
	               httpheader=myheader,
	               verbose=FALSE)		
	xmltext  <- xmlTreeParse(data, asText = TRUE,useInternalNodes=TRUE)
	response <- xmlRoot(xmltext)
	responseNS <- xmlNamespaceDefinitions(response, simplify = TRUE)  
	processID <- xmlGetAttr(response,"statusLocation")
	
	.Object	<-	setProcessID(.Object,processID)
	return(.Object)
})


# '@rdname checkProcess-methods
# '@aliases checkProcess,rGDP-method
setMethod(f = "checkProcess",signature = "rGDP",definition = function(.Object){
	
	process	<-	list(status=NULL,URL=NULL)
	if (.Object@processID=="Null"){
		process$status	<-	'none'
	}

	tryCatch({checkForComplete=getURL(url = .Object@processID, verbose=FALSE)},error = function(e) {process$status='unknown'})
	if (is.null(process$status)){
		checkForCompleteResponse	<-	xmlTreeParse(checkForComplete, asText = TRUE,useInternalNodes=TRUE)
		checkResponseNS <- xmlNamespaceDefinitions(checkForCompleteResponse, simplify = TRUE) 
		root <- xmlRoot(checkForCompleteResponse)
		status <- sapply(xmlChildren(root[["Status"]]),xmlValue)
		process$status	<-	status[[1]]
		if ("Process successful" == process$status){
			root <- xmlRoot(checkForCompleteResponse)
		    process$URL <- as.character(xpathApply(root, "//@href", namespaces = checkResponseNS)[[1]])
		}
	}

	return(process)
})

# create summary method......

setMethod(f = "print",signature = "rGDP",
	function(x,...){
		cat("*** Class rGDP, method Print *** \n")
		cat("* WFS_URL:\t");cat(x@WFS_URL,"\n")
		cat("* WPS_URL:\t");cat(x@WPS_URL,"\n")
		cat("* algorithm:\t");cat(names(x@algorithm),"\n")
		cat("* ------postInputs------\n")
		PI	<-	x@postInputs
		PI[is.na(PI)] = '[optional]'
		nms	<-	names(PI)		
		for (i in 1:length(nms)){cat("\t-",nms[i]);cat(":",PI[[i]],"\n")}
		cat("* ------feature------\n")
		PI	<-	x@feature
		nms	<-	names(PI)
		PI[is.na(PI)] = '[optional]'
		for (i in 1:length(nms)){
			# will skip "hidden"
			if (!is.null(PI[[i]]) && PI[[i]]!='hidden'){
				cat("\t-",nms[i]);cat(":",PI[[i]],"\n")
			} else if (is.null(PI[[i]])){
				cat("\t-",nms[i]);cat(":",PI[[i]],"\n")
			}
		}
		cat("* processID:\t");cat(x@processID,"\n")
		cat("**** End Print (rGDP)**** \n")
	}
)
