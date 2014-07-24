#'@details checkProcess is a method for checking the process status of an active (executed)
#' \code{geoknife} object. The method returns \code{process}, which is a list containing
#' two fields: \code{status} and \code{URL}. If the \code{geoknife} object has not been executed
#' (see \code{startProcess}), this method returns \code{status}='none' and \code{URL}=NULL.
#'
#'@param \code{geoknife} object with an active GDP process request.
#'@return \code{process}, a list containing
#' \code{status} and \code{URL}. 
#'
#'@docType methods
#'@keywords checkProcess
#'@description Check status of processing request
#'@title Check status of processing request
#'@author Jordan S. Read
#'@seealso \code{startProcess}
#'@import XML
#'@import RCurl
#'@examples 
#'gk <- geoknife() # create geoknife object
#'checkProcess(gk) # no process for empty geoknife object
#'@export
setGeneric(name="checkProcess",def=function(.Object){standardGeneric("checkProcess")})

# '@rdname checkProcess-methods
# '@aliases checkProcess,geoknife-method
setMethod(f = "checkProcess",signature = "geoknife",definition = function(.Object){
	
	process	<-	list(status=NULL,URL=NULL)
	if (.Object@processID=="<no active job>"){
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