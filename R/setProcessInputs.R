#'@title set inputs for web processing
#'
#'@details method for setting the (non-feature related) post inputs of the \code{geoknife} object. 
#'
#'@param An \code{geoknife} object.
#'@param a list of valid processInputs.
#'@return An \code{geoknife} object with updated postInputs.
#'@docType methods
#'@keywords setProcessInputs

#'@export
setGeneric(name="setProcessInputs<-",def=function(.Object,value){standardGeneric("setProcessInputs<-")})


# '@rdname setProcessInputs-methods
# '@aliases setProcessInputs,geoknife-method	
setReplaceMethod(f = "setProcessInputs",signature = "geoknife",
	definition = function(.Object,value){
		
    
		if ("empty" %in% names(.Object@algorithm)){
			stop('an algorithm must be chosen before setting processInputs')
		}
		for (i in seq_len(length(names(value)))){
			.Object@processInputs[names(value[i])]	<-	value[[i]]
		}
		
		.Object	<-	dodsReplace(.Object)
		
		return(.Object)
	})
	
dodsReplace	<-	function(.Object){
	# checks for dods or opendap, and replaces
	
			
	if ("DATASET_URI" %in% names(.Object@processInputs) & 
		!is.null(.Object@processInputs$DATASET_URI)) {
		
		uri	<-	.Object@processInputs$DATASET_URI
		if (grepl('dodsC',uri)){
			uri	<-	gsub('http', 'dods', uri)
		}
		if (grepl('opendap',uri)){
			uri	<-	gsub('http', 'opendap', uri)
		}

		.Object@processInputs	<-	setList(.Object@processInputs,list('DATASET_URI'=uri))
	}
	return(.Object)
}