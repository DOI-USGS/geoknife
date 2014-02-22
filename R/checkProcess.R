#'checkProcess
#'
#'method for checking the process status of an active (executed) \code{rGDP} object. 
#'
#'@param \code{rGDP} object with an active GDP process request.
#'@return process of \code{rGDP} process.
#'@docType methods
#'@keywords checkProcess
#'@title Checks status of processing request
#'@export
setGeneric(name="checkProcess",def=function(.Object){standardGeneric("checkProcess")})

# '@rdname checkProcess-methods
# '@aliases checkProcess,rGDP-method
setMethod(f = "checkProcess",signature = "rGDP",definition = function(.Object){
	
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