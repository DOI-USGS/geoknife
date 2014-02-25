#'@title set processing algorithm
#'
#'@details method for setting the process algorithm of the \code{geoknife} object.
#'
#'@param An \code{geoknife} object.
#'@param a list for a valid algorithm, including values for name & location
#'@return An \code{geoknife} object.
#'@docType methods
#'@keywords setAlgorithm
#'@examples gk <- geoknife() # create geoknife object
#'algorithm <- list("Area Grid Statistics (weighted)"=
#'"gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
#'gk <- setAlgorithm(gk,algorithm)
#'@export
setGeneric(name="setAlgorithm<-",def=function(.Object,value){standardGeneric("setAlgorithm<-")})



# '@rdname setAlgorithm-methods
# '@aliases setAlgorithm,geoknife-method	
setReplaceMethod(f = "setAlgorithm",signature = "geoknife",
	definition = function(.Object,value){
		.Object@algorithm	<-	value
		# now, initialize posts
		.Object	<-	initializeProcessInputs(.Object)
		return(.Object)
	})