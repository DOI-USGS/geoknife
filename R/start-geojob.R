#' start a geo-web processing request
#'
#'  start is a method for submitting a geo-web processing request.
#'
#' @param .Object a \linkS4class{geojob} object
#' @return A \linkS4class{geojob} object with an active GDP process request.
#'
#' @docType methods
#' @description Start process for \code{\link{geojob}}
#' @title Submit a GDP web processing request
#' @seealso \code{\link{check}}
#' @aliases start
#' @docType methods
#' @export
#' @rdname start-methods
#' @examples 
#' wd <- webdata('prism')
#' wg <- webgeom('state::New Hampshire')
#' wp <- webprocess()
#' gj <- geojob()
#' \dontrun{
#' xml(gj) <- XML(wg, wd, wp)
#' url(gj) <- url(wp)
#' gj <- start(gj)
#' }
setGeneric(name="start",def=function(.Object){standardGeneric("start")})

#'@rdname start-methods
#'@export
setMethod(f = "start",signature(.Object = "geojob"),definition = function(.Object){
	
  if (!canStart()){
    stop('Cannot start a new geojob until a previous one is completed or is error. ',
         'See "check(geojob)"', call.=FALSE)
  }
	requestXML <- xml(.Object)
	data <- genericExecute(url = url(.Object), requestXML)
  
	xmltext <- xmlTreeParse(data, asText = TRUE,useInternalNodes=TRUE)
	response <- xmlRoot(xmltext)
	responseNS <- xmlNamespaceDefinitions(response, simplify = TRUE)  
	processID <- xmlGetAttr(response,"statusLocation")
	
	id(.Object)	<-	processID
  setJobState("ProcessStarted")
	return(.Object)
})
