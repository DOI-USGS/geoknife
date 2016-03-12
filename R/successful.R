#'@title Convenience function for GDP process state
#'@rdname successful-methods
#'@aliases
#'successful
#'running
#'error
#'@usage
#'successful(.Object, retry)
#'error(.Object, retry)
#'running(.Object, retry)
#'
#'@param .Object a \linkS4class{geojob} object
#'@param retry attempt to retry again if communication failed with the server
#'@return TRUE/FALSE indicating if process is in the given state (error, processing, successful)
#'@description Simple wrapper to check process status
#'
#'@author Luke Winslow, Jordan S Read
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
setGeneric(name="successful",def=function(.Object, retry){standardGeneric("successful")})


#'@rdname successful-methods
#'@aliases successful
setMethod(f = "successful",signature(.Object = "geojob"), definition = function(.Object, retry = FALSE){
	
  process = check(.Object)
  if (process$status == 'unknown' && retry){
    Sys.sleep(gconfig('sleep.time'))
    process = check(.Object)
  }
	
	return(process$statusType == "ProcessSucceeded")
	
})


#'@export
setGeneric(name="running",def=function(.Object, retry){standardGeneric("running")})

#'@rdname successful-methods
#'@aliases running
setMethod(f = "running",signature(.Object = "geojob"), definition = function(.Object, retry = FALSE){
  
  process = check(.Object)
  if (process$status == 'unknown' && retry){
    Sys.sleep(gconfig('sleep.time'))
    process = check(.Object)
  }
  
  return(process$statusType == "ProcessStarted" | process$statusType == "ProcessAccepted")
})


#'@export
setGeneric(name="error",def=function(.Object, retry){standardGeneric("error")})

#'@rdname successful-methods
#'@aliases error
setMethod(f = "error",signature = "geojob", definition = function(.Object, retry = FALSE){
  
  process = check(.Object)
  if (process$status == 'unknown' && retry){
    Sys.sleep(gconfig('sleep.time'))
    process = check(.Object)
  }
  
  
  return(process$statusType == "ProcessFailed")
})