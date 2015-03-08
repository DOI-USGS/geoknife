
#'@export
setGeneric(name="error",def=function(.Object){standardGeneric("error")})

# '@rdname error-methods
# '@aliases error,geojob-method
setMethod(f = "error",signature = "geojob", definition = function(.Object){
	
	status = check(.Object)
	
	return(status$statusType == "ProcessFailed")
})