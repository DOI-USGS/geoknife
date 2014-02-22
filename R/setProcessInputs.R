#'setProcessInputs
#'
#'method for setting the (non-feature related) post inputs of the \code{rGDP} object. 
#'
#'@param An \code{rGDP} object.
#'@param a list of valid processInputs.
#'@return An \code{rGDP} object.
#'@docType methods
#'@keywords setProcessInputs
#'@export
setGeneric(name="setProcessInputs",def=function(.Object,processInputs){standardGeneric("setProcessInputs")})


# '@rdname setProcessInputs-methods
# '@aliases setProcessInputs,rGDP-method	
setMethod(f = "setProcessInputs",signature = "rGDP",
	definition = function(.Object,processInputs){
		if ("empty" %in% names(.Object@algorithm)){
			stop('an algorithm must be chosen before setting processInputs')
		}
		
		if (("DATASET_URI" %in% names(processInputs)) & 
			!is.null(processInputs["DATASET_URI"]) & 
			grepl('dodsC',processInputs["DATASET_URI"])){
			processInputs["DATASET_URI"]	<-	gsub('http', 'dods', processInputs["DATASET_URI"])
		}
		if (("DATASET_URI" %in% names(processInputs)) & 
			!is.null(processInputs["DATASET_URI"]) & 
			grepl('opendap',processInputs["DATASET_URI"])){
			processInputs["DATASET_URI"]	<-	gsub('http', 'opendap', processInputs["DATASET_URI"])
		}
		.Object@processInputs	<-	setList(.Object@processInputs,processInputs)
		if ("LinearRing" %in% names(.Object@feature) && "FEATURE_ATTRIBUTE_NAME" %in% names(.Object@processInputs)){
			.Object@processInputs$FEATURE_ATTRIBUTE_NAME	<-	'the_geom'
		}
		return(.Object)
	})