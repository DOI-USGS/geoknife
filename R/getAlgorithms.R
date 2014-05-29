#'@details getAlgorithms is a method for finding algorithm names and locations for
#' a \code{geoknife} object.
#'
#'@param \code{geoknife} object with a valid WPS url.
#'@return list of available algorithms for the \code{geoknife} WPS url.
#'@docType methods
#'@description Get algorithms for \code{geoknife}
#'@title Get processing algorithms
#'@keywords getAlgorithms
#'@seealso \code{setAlgorithm<-}
#'@examples gk <- geoknife() # create geoknife object
#'getAlgorithms(gk) # list Geo Data Portal processing algorithms
#'@export
setGeneric(name="getAlgorithms",def=function(.Object){standardGeneric("getAlgorithms")})

# '@rdname getAlgorithms-methods
# '@aliases getAlgorithms,geoknife-method	
setMethod(f = "getAlgorithms",signature="geoknife",
	definition = function(.Object){
		processURL	<-	paste(c(.Object@WPS_URL,'?service=WPS&version=',
			.Object@WPS_DEFAULT_VERSION,'&request=GetCapabilities'),collapse="")
		algorithm.Loc	<-	parseXMLnodes(processURL,"process","identifier",key=NA)
		algorithm.Nm	<-	parseXMLnodes(processURL,"process","title",key=NA)
		algorithms	<-	list()
		for (i in 1:length(algorithm.Nm)){
			algorithms[[i]]	<-	algorithm.Loc[i]
		}
		names(algorithms)	<-	algorithm.Nm
		return(algorithms)
	})