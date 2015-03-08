#'@title Convenience function for GDP process state
#'@aliases
#'successful
#'running
#'error
#'
#'@usage
#'successful(.Object)
#'error(.Object)
#'running(.Object)
#'
#'@param .Object a \code{\link{geojob}} object
#'@return TRUE/FALSE indicating if process is in the given state (error, processing, successful)
#'@description Simple wrapper to check process status
#'
#'@author Luke Winslow
#'@seealso \code{\link{check}}
#'
#'@examples
#'\dontrun{
#'wp <- quick_wp(url = 'http://cida-test.er.usgs.gov/gdp/process/WebProcessingService')
#'wd <- quick_wd()
#'wg <- quick_wg()
#'gj <- geojob()
#'xml(gj) <- XML(wg, wd, wp)
#'url(gj) <- url(wp)
#'gj <- start(gj)
#'
#'running(gk)
#'error(gk)
#'successful(gk)
#'}
#'
#'@export
setGeneric(name="successful",def=function(.Object){standardGeneric("successful")})


# '@rdname successful-methods
# '@aliases successful,geoknife-method
setMethod(f = "successful",signature = "geojob", definition = function(.Object){
	
	status = check(.Object)
	
	return(status$statusType == "ProcessSucceeded")
	
})
	