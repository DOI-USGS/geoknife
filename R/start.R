#'@details startProcess is a method for submitting a web processing request for the \code{geoknife} object.
#'
#'@param .Object a \code{geoknife} object ot be used to formulate GDP process request.
#'@return An \code{geoknife} object with an active GDP process request.
#'
#'@docType methods
#'@keywords startProcess
#'@description Start process for \code{geojob}
#'@title Submit a GDP web processing request
#'@seealso \code{check}
#'@export
#'@examples 
#'wp <- quick_wp()
#'wd <- quick_wd()
#'wg <- quick_wg()
#'gj <- geojob()
#'xml(gj) <- XML(wg, wd, wp)
#'url(gj) <- wp@@wps_url
#'gj <- start(gj)
#'id(gj) # get the processing ID for the geojob
#'# -- is execute true (geoknife) or "start(geojob)"
setGeneric(name="start",def=function(.Object){standardGeneric("start")})

# '@rdname startProcess-methods
# '@aliases startProcess,geoknife-method
setMethod(f = "start",signature = "geojob",definition = function(.Object){
	
	requestXML	<-	xml(.Object)
	data <- genericExecute(url = url(.Object), requestXML)
  
	xmltext 	<-	xmlTreeParse(data, asText = TRUE,useInternalNodes=TRUE)
	response	<-	xmlRoot(xmltext)
	responseNS	<-	xmlNamespaceDefinitions(response, simplify = TRUE)  
	processID	<-	xmlGetAttr(response,"statusLocation")
	
	id(.Object)	<-	processID
	return(.Object)
})
