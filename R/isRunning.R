
#'@export
setGeneric(name="isRunning",def=function(.Object){standardGeneric("isRunning")})

# '@rdname isRunning-methods
# '@aliases isRunning,geoknife-method
setMethod(f = "isRunning",signature = "geoknife", definition = function(.Object){
	
	status = checkProcess(.Object)
	
	return(status$statusType == "ProcessStarted")
})