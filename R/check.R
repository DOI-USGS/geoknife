#'@details check is a method for checking the process status of an active (executed)
#' \linkS4class{geojob} object. The method returns \code{process}, which is a list containing
#' two fields: \code{status} and \code{URL}. If the \linkS4class{geojob} object has not been executed
#' (see \code{\link{start}}), this method returns \code{status}='none' and \code{URL}=NULL.
#'
#'@param .Object a \linkS4class{geojob} object with an active GDP process request.
#'@return \code{process}, a list containing
#' \code{status} and \code{URL}. 
#'
#'@description Check status of processing request
#'@title Check status of processing request
#'@aliases check
#'@author Jordan S. Read
#'@seealso \code{\link{start}}
#'@importFrom XML xmlTreeParse xmlNamespaceDefinitions xmlRoot
#'@rdname check-geojob
#'@examples 
#'gj <- geojob() # create geojob object
#'check(gj) # no process for empty geojob object
#'@export            
setGeneric(name="check",def=function(.Object){standardGeneric("check")})

#'@rdname check-geojob
#'@aliases check
setMethod(f = "check",signature(.Object = "geojob"), definition = function(.Object){

	process	<-	list(status=NULL,URL=NULL)
	if (id(.Object)=="<no active job>"){
		process$status	<-	'none'
		process$statusType <- 'none'
    return(process)
	}

	checkForComplete = tryCatch({
    gGET(url = id(.Object))
    },error = function(e) {
      return(NULL)
      }
    )
  if (is.null(checkForComplete)){
    process$status <- 'unknown'
    process$statusType <- 'unknown'
  }
	if (is.null(process$status)){
		checkForCompleteResponse <- gcontent(checkForComplete)
		checkResponseNS <- xmlNamespaceDefinitions(checkForCompleteResponse, simplify = TRUE) 
		root <- xmlRoot(checkForCompleteResponse)
		status <- sapply(xmlChildren(root[["Status"]]),xmlValue)
		process$status	<-	status[[1]]
		process$statusType <- sapply(xmlChildren(root[["Status"]]),xmlName)[[1]]
		
		if (process$status == "Process successful"){
			root <- xmlRoot(checkForCompleteResponse)
			process$URL <- as.character(xpathApply(root, "//@href", namespaces = checkResponseNS)[[1]])
		} else if (process$status == ""){
		  process$status <- "ProcessStarted"
		} else if (substr(process$status, 1, 34) == "org.n52.wps.server.ExceptionReport"){
		  process$status <- "ProcessFailed"
		}
	}
  
  setJobState(process$status)
	return(process)
})