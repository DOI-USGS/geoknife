
#'@export
setGeneric(name="isError",def=function(.Object){standardGeneric("isError")})

# '@rdname isError-methods
# '@aliases isError,geoknife-method
setMethod(f = "isError",signature = "geoknife", definition = function(.Object){
	
	status = checkProcess(.Object)
	
	return(grepl('org.n52.wps.server.ExceptionReport', status$status))
	
})