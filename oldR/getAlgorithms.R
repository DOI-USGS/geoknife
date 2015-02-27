#'@details algorithms is a method for finding algorithm names and locations for
#' a \code{geoknife} object.
#'
#'@param .Object a \code{geoknife} object with a valid WPS url.
#'@return list of available algorithms for the \code{geoknife} WPS url.
#'@docType methods
#'@description Get algorithms for \code{geoknife}
#'@title Get processing algorithms
#'@keywords methods
#'@author Jordan S. Read
#'@seealso \code{setAlgorithm<-}
#'@examples 
#'wp <- webprocess() # create geoknife object
#'algorithms(wp)
#'algorithms() # list Geo Data Portal processing algorithms
#'@export
setGeneric(name="algorithms",def=function(.Object){standardGeneric("algorithms")})

# '@rdname algorithms-methods
# '@aliases algorithms,webprocess-method 	
setMethod(f = "algorithms",signature="webprocess",
	definition = function(.Object){
		
	  algorithmParse(wps_url = .Object@wps_url, wps_version = .Object@WPS_VERSION)
	})

# '@rdname algorithms-methods
# '@aliases algorithms,webprocess-method  
setMethod(f = "algorithms",signature="missing",
          definition = function(.Object){
            algorithms(webprocess())
          }
)

# '@rdname algorithms-methods
# '@aliases algorithms,geojob-method  
setMethod(f = "algorithms",signature="geojob",
          definition = function(.Object){
            algorithmParse(wps_url = .Object@WPS_URL, wps_version = .Object@WPS_DEFAULT_VERSION)
          }
)

setMethod(f = "algorithms",signature="character",
          definition = function(.Object){
            algorithmParse(processURL = .Object)
          }
)

#'@importFrom httr GET
algorithmParse = function(wps_url, wps_version, processURL){
  
  if (missing(processURL)){
    processURL  <-  paste(c(wps_url,'?service=WPS&version=',
                            wps_version,'&request=GetCapabilities'),collapse="")
  }
  
  doc <- GET(processURL)
  algorithm.Loc	<-	parseXMLnodes(doc,"process","identifier",key=NA)
  algorithm.Nm	<-	parseXMLnodes(doc,"process","title",key=NA)
  algorithms	<-	list()
  for (i in 1:length(algorithm.Nm)){
    algorithms[[i]]	<-	algorithm.Loc[i]
  }
  names(algorithms)	<-	algorithm.Nm
  return(algorithms)
}