
#'@export
setGeneric(name="running",def=function(.Object){standardGeneric("running")})

# '@rdname running-methods
# '@aliases running,geoknife-method
setMethod(f = "running",signature = "geojob", definition = function(.Object){
	
	status = check(.Object)
	
	return(status$statusType == "ProcessStarted")
})