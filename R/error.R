
#'@export
setGeneric(name="isError",def=function(.Object){standardGeneric("isError")})

# '@rdname isError-methods
# '@aliases isError,geoknife-method
setMethod(f = "isError",signature = "geojob", definition = function(.Object){
	
	status = checkProcess(.Object)
	
	return(status$statusType == "ProcessFailed")
})