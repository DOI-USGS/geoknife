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
		
		.Object@processInputs	<-	setList(.Object@processInputs,processInputs)
		if ("LinearRing" %in% names(.Object@feature) && "FEATURE_ATTRIBUTE_NAME" %in% names(.Object@processInputs)){
			.Object@processInputs$FEATURE_ATTRIBUTE_NAME	<-	'the_geom'
		}
		.Object	<-	dodsReplace(.Object)
		
		return(.Object)
	})
	
dodsReplace	<-	function(.Object){
	# checks for dods or opendap, and replaces
	if ("DATASET_URI" %in% names(.Object@processInputs) & 
		!is.null(.Object@processInputs$DATASET_URI)) {
		uri	<-	.Object@processInputs$DATASET_URI
		uri	<-	gsub('http', 'dods', uri)
		uri	<-	gsub('http', 'opendap', uri)
		.Object@processInputs	<-	setList(.Object@processInputs,list('DATASET_URI'=uri))
	}
	return(.Object)
}