#'@title start a geo-web processing request
#'@details start is a method for submitting a geo-web processing request.
#'
#'@param .Object a \linkS4class{geojob} object
#'@return A \linkS4class{geojob} object with an active GDP process request.
#'
#'@docType methods
#'@keywords methods
#'@description Start process for \code{geojob}
#'@title Submit a GDP web processing request
#'@seealso \code{check}
#'@aliases start
#'@docType methods
#'@export
#'@rdname start-methods
#'@examples 
#'wp <- quick_wp()
#'wd <- webdata('prism')
#'wg <- quick_wg()
#'gj <- geojob()
#'xml(gj) <- XML(wg, wd, wp)
#'url(gj) <- url(wp)
#'gj <- start(gj)
setGeneric(name="start",def=function(.Object){standardGeneric("start")})

#'@rdname start-methods
#'@export
setMethod(f = "start",signature(.Object = "geojob"),definition = function(.Object){
	
	requestXML <- xml(.Object)
	data <- genericExecute(url = url(.Object), requestXML)
  
	xmltext <- xmlTreeParse(data, asText = TRUE,useInternalNodes=TRUE)
	response <- xmlRoot(xmltext)
	responseNS <- xmlNamespaceDefinitions(response, simplify = TRUE)  
	processID <- xmlGetAttr(response,"statusLocation")
	
	id(.Object)	<-	processID
	return(.Object)
})
