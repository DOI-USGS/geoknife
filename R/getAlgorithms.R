#'getAlgorithms
#'
#'a \code{rGDP} method for finding algorithm names and locations from a valid WPS endpoint.
#'
#'@param \code{rGDP} object with a valid WPS url.
#'@return list of available algorithms for the \code{rGDP} WPS url.
#'@docType methods
#'@keywords getAlgorithms
#'@export
setGeneric(name="getAlgorithms",def=function(.Object){standardGeneric("getAlgorithms")})

# '@rdname getAlgorithms-methods
# '@aliases getAlgorithms,rGDP-method	
setMethod(f = "getAlgorithms",signature="rGDP",
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