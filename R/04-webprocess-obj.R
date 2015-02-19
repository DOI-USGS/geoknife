setClass(
  Class = "webprocess",
  representation = representation(
    wps_url="character",
    algorithm="list",
    processInputs="list",
    WPS_VERSION="character",
    WPS_SCHEMA_LOCATION="character",
    WPS_NAMESPACE="character",
    OWS_NAMESPACE="character",
    XSI_SCHEMA_LOCATION="character",
    XSI_NAMESPACE = "character",
    UTILITY_URL = "character",
    OGC_NAMESPACE="character",
    emailK="character")
)

setMethod(f="initialize",signature="webprocess",
          definition=function(.Object, wps_url, algorithm){
            # public variables (available via get and set methods)  
            if (missing(wps_url)){
              wps_url = 'http://cida.usgs.gov/gdp/process/WebProcessingService'
            }
            if (missing(algorithm)){
              algorithm = list('Area Grid Statistics (weighted)'=
                                 "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
            }
            
            .Object@WPS_VERSION = '1.0.0'
            .Object@WPS_SCHEMA_LOCATION = 'http://schemas.opengis.net/wps/1.0.0/wpsExecute_request.xsd'
            .Object@WPS_NAMESPACE ='http://www.opengis.net/wps/1.0.0'
            
            .Object@XSI_SCHEMA_LOCATION = 'http://www.opengis.net/wfs ../wfs/1.1.0/WFS.xsd'
            .Object@XSI_NAMESPACE = 'http://www.w3.org/2001/XMLSchema-instance'
            
            .Object@OGC_NAMESPACE = 'http://www.opengis.net/ogc'
            
            .Object@OWS_NAMESPACE = 'http://www.opengis.net/ows/1.1'
            .Object@UTILITY_URL = 'http://cida.usgs.gov/gdp/utility/WebProcessingService'
            
            .Object@emailK      = 'gov.usgs.cida.gdp.wps.algorithm.communication.EmailWhenFinishedAlgorithm'
            
            .Object@wps_url = wps_url
            .Object@algorithm  <- algorithm
            processInputs <- defaultProcessInputs(algorithm = .Object@algorithm[[1]], .Object@wps_url, .Object@WPS_VERSION)
            .Object@processInputs  <-	processInputs
            
            return(.Object)
          })

#' create webprocess object
#' @description A class representing geoknife web processing specifications
#'
#' @return the webprocess object
#' @author Jordan S Read
#' @rdname webprocess-methods
#' @export
setGeneric("webprocess", function(...) {
  standardGeneric("webprocess")
})

#'@rdname webprocess-methods
#'@aliases webprocess,webprocess-method
setMethod("webprocess", signature(), function(...) {
  ## create new webprocess object
  webprocess <- new("webprocess",...)
  return(webprocess)
})

#'@export
setGeneric(name="algorithm",def=function(.Object){
  standardGeneric("algorithm")
})

#'@rdname webprocess-methods
#'@aliases algorithm,webprocess-method
setMethod(f = "algorithm",signature="webprocess",
          definition = function(.Object){
            return(.Object@algorithm)
          }
)

#'@export
setGeneric(name="algorithm<-",def=function(.Object, value){
  standardGeneric("algorithm<-")
})

#'@rdname webprocess-methods
#'@aliases algorithm<-,webprocess-method
setMethod(f = "algorithm<-",signature = "webprocess",
                 definition = function(.Object,value){
                   .Object <- initialize(.Object, algorithm = value)
                   return(.Object)
                 })

