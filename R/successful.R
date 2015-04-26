#'@title Convenience function for GDP process state
#'@rdname successful-methods
#'@aliases
#'successful
#'running
#'error
#'@usage
#'successful(.Object)
#'error(.Object)
#'running(.Object)
#'
#'@param .Object a \linkS4class{geojob} object
#'@return TRUE/FALSE indicating if process is in the given state (error, processing, successful)
#'@description Simple wrapper to check process status
#'
#'@author Luke Winslow
#'@seealso \code{\link{check}}
#'
#'@examples
#'\dontrun{
#'wp <- quick_wp()
#'job <- geoknife(stencil = c(-89,42), fabric = 'prism', knife = wp)
#'check(job)
#'
#'running(job)
#'error(job)
#'successful(job)
#'}
#'
#'@export
setGeneric(name="successful",def=function(.Object){standardGeneric("successful")})


#'@rdname successful-methods
#'@aliases successful
setMethod(f = "successful",signature(.Object = "geojob"), definition = function(.Object){
	
	status = check(.Object)
	
	return(status$statusType == "Process successful")
	
})


#'@export
setGeneric(name="running",def=function(.Object){standardGeneric("running")})

#'@rdname successful-methods
#'@aliases running
setMethod(f = "running",signature(.Object = "geojob"), definition = function(.Object){
  
  status = check(.Object)
  
  return(status$statusType == "ProcessStarted")
})


#'@export
setGeneric(name="error",def=function(.Object){standardGeneric("error")})

#'@rdname successful-methods
#'@aliases error
setMethod(f = "error",signature = "geojob", definition = function(.Object){
  
  status = check(.Object)
  
  return(status$statusType == "ProcessFailed")
})