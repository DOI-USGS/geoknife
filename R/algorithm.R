#'@title set processing algorithm
#'
#'@details method for setting the process algorithm of the \code{geoknife} object.
#'
#'@param .Object a \code{geoknife} object.
#'@param value a list for a valid algorithm, including values for name & location
#'@return An \code{geoknife} object.
#'@docType methods
#'@keywords methods
#'@examples 
#'gk <- geoknife() # create geoknife object
#'algorithm <- list("Area Grid Statistics (weighted)"=
#'"gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
#'algorithm(gk) <- algorithm
#'@author Jordan S. Read
#'@rdname algorithm-set
#'@export
setGeneric(name="algorithm<-",def=function(.Object,value){
  standardGeneric("algorithm<-")
})


#'@rdname algorithm-set
#'@aliases algorithm<-
setReplaceMethod(f = "algorithm",signature = "geoknife",
	definition = function(.Object,value){
		.Object@algorithm	<-	value
		# now, initialize posts
		.Object	<-	initializeProcessInputs(.Object)
		return(.Object)
	})

#'@title get processing algorithm
#'
#'@details method for getting the process algorithm of the \code{geoknife} object.
#'
#'@param .Object a \code{geoknife} object.
#'@return An \code{geoknife} object's algorithm
#'@keywords methods
#'@examples 
#'gk <- geoknife() # create geoknife object
#'algorithm <- list("Area Grid Statistics (weighted)"=
#'"gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
#'algorithm(gk) <- algorithm
#'algorithm(gk)
#'@author Jordan S. Read
#'@rdname algorithm-methods
#'@name algorithm
#'@docType methods
#'@export
setGeneric(name="algorithm",def=function(.Object){
  standardGeneric("algorithm")
})

#'@rdname algorithm-methods
#'@aliases algorithm,geoknife-method
setMethod(f = "algorithm",signature="geoknife",
          definition = function(.Object){
            return(.Object@algorithm)
          }
)
