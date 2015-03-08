#'@details start is a method for submitting a geo-web processing request.
#'
#'@param .Object a \code{\link{geojob}} object
#'@return A \code{\link{geojob}} object with an active GDP process request.
#'
#'@docType methods
#'@keywords methods
#'@description Start process for \code{geojob}
#'@title Submit a GDP web processing request
#'@seealso \code{check}
#'@export
#'@examples 
#'wp <- quick_wp(url = 'http://cida-test.er.usgs.gov/gdp/process/WebProcessingService')
#'wd <- quick_wd()
#'wg <- quick_wg()
#'gj <- geojob()
#'xml(gj) <- XML(wg, wd, wp)
#'url(gj) <- url(wp)
#'gj <- start(gj)
setGeneric(name="start",def=function(.Object){standardGeneric("start")})

# '@rdname start-methods
# '@aliases start,geoknife-method
setMethod(f = "start",signature = "geojob",definition = function(.Object){
	
	requestXML <- xml(.Object)
	data <- genericExecute(url = url(.Object), requestXML)
  
	xmltext <- xmlTreeParse(data, asText = TRUE,useInternalNodes=TRUE)
	response <- xmlRoot(xmltext)
	responseNS <- xmlNamespaceDefinitions(response, simplify = TRUE)  
	processID <- xmlGetAttr(response,"statusLocation")
	
	id(.Object)	<-	processID
	return(.Object)
})
