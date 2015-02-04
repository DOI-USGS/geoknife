#'@title Convenience function for GDP process state
#'@aliases
#'isSuccessful
#'isRunning
#'isError
#'
#'@usage
#'isSuccessful(.Object)
#'isError(.Object)
#'isRunning(.Object)
#'
#'@param .Object a \code{geoknife} object with an active GDP process request.
#'@return TRUE/FALSE indicating if process is in the given state (error, processing, successful)
#'@description Simple wrapper to check process status
#'
#'@author Luke Winslow
#'@seealso \code{checkProcess}
#'
#'@examples
#'
#'gk <- geoknife() # create geoknife object
#'gk # print geoknife object
#'
#'linearRing = bufferPoint(c(-111.48,36.95))
#'setFeature(gk) <-list(LinearRing=linearRing)
#'
#'#get a list of available processing algorithms
#'getAlgorithms(gk)
#'
#'#set processing algorithm to feature weighted grid statistics (unweighted will likely fail, because the ring won't intersect the centroids)
#'setAlgorithm(gk) <- getAlgorithms(gk)[4] # feature weighted
#'
#'# set the post inputs for the processing dataset
#'setProcessInputs(gk) <- list('DATASET_ID'='prcp',
#'														 'DATASET_URI'='http://thredds.daac.ornl.gov/thredds/dodsC/daymet-agg/daymet-agg.ncml',
#'														 'TIME_START'='2010-01-01T00:00:00Z',
#'														 'TIME_END'='2010-01-03T00:00:00Z',
#'														 'DELIMITER'='TAB')
#'gk # print geoknife object contents
#'
#'# kick off your request
#'gk <- startProcess(gk)
#'
#'isRunning(gk)
#'isError(gk)
#'isSuccessful(gk)
#'
#'
#'@export
setGeneric(name="isSuccessful",def=function(.Object){standardGeneric("isSuccessful")})


# '@rdname isSuccessful-methods
# '@aliases isSuccessful,geoknife-method
setMethod(f = "isSuccessful",signature = "geoknife", definition = function(.Object){
	
	status = checkProcess(.Object)
	
	return(status$statusType == "ProcessSucceeded")
	
})
	