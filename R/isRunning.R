
#'@export
setGeneric(name="isRunning",def=function(.Object){standardGeneric("isRunning")})

# '@rdname isRunning-methods
# '@aliases isRunning,geoknife-method
setMethod(f = "isRunning",signature = "geoknife", definition = function(.Object){
	
	#TODO: Optimize this so it doesn't make 3 calls to GDP
	status = checkProcess(.Object)
	
	#if it isn't either of these, then it must still be processing
	if(!isError(.Object) & !isSuccessful(.Object)){
		
		#If the status is none, the job hasn't be submitted
		return(status$status != 'none')
	}
})