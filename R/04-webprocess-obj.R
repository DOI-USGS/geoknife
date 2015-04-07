#' @title webprocess class
#' @description A class representing geoknife web processing specifications
#' @slot url URL for webprocessing service. 
#' Can be set or accessed using \code{\link[geoknife]{url}}
#' @slot algorithm a list for algorithm used. 
#' Can be set or accessed using \code{\link[geoknife]{algorithm}}
#' @slot processInputs (_private) a list of required and options process inputs, and their 
#' default values (if specified). This is populated (or repopulated) whenever \code{algorithm} is set.
#' @slot WPS_VERSION (_private) a character specifying the wep processing service version to use. 
#' @slot WPS_SCHEMA_LOCATION (_private) location for web processing service schema
#' @slot WPS_NAMESPACE (_private) location for web processing service namespace
#' @slot OWS_NAMESPACE (_private) namespace web location 
#' @slot XSI_SCHEMA_LOCATION (_private) schema web location
#' @slot XSI_NAMESPACE (_private) namespace web location
#' @slot XLINK_NAMESPACE (_private) namespace web location
#' @slot UTILITY_URL (_private) web processing service utility url. Uses same base url as
#' public slot \code{url}
#' @slot OGC_NAMESPACE (_private) namespace web location
#' @slot emailK (_private) relative url for email when complete utility. 
#' @rdname webprocess-class
#' @aliases
#' webprocess-class
#' @exportClass webprocess
setClass(
  Class = "webprocess",
  prototype = prototype(
    url = 'http://cida.usgs.gov/gdp/process/WebProcessingService', 
    algorithm = list('Area Grid Statistics (weighted)'=
                       "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")
    ),
  representation = representation(
    url="character",
    algorithm="list",
    processInputs="list",
    WPS_VERSION="character",
    WPS_SCHEMA_LOCATION="character",
    WPS_NAMESPACE="character",
    OWS_NAMESPACE="character",
    XSI_SCHEMA_LOCATION="character",
    XSI_NAMESPACE = "character",
    XLINK_NAMESPACE = "character",
    UTILITY_URL = "character",
    OGC_NAMESPACE="character",
    emailK="character")
)

setMethod(f="initialize",signature="webprocess",
          definition=function(
            .Object, 
            url = .Object@url, 
            algorithm = .Object@algorithm)
            {
            .Object@WPS_VERSION = '1.0.0'
            .Object@WPS_SCHEMA_LOCATION = 'http://schemas.opengis.net/wps/1.0.0/wpsExecute_request.xsd'
            .Object@WPS_NAMESPACE ='http://www.opengis.net/wps/1.0.0'
            
            .Object@XSI_SCHEMA_LOCATION = 'http://www.opengis.net/wfs ../wfs/1.1.0/WFS.xsd'
            .Object@XSI_NAMESPACE = 'http://www.w3.org/2001/XMLSchema-instance'
            
            .Object@OGC_NAMESPACE = 'http://www.opengis.net/ogc'
            .Object@XLINK_NAMESPACE = 'http://www.w3.org/1999/xlink'
            
            .Object@OWS_NAMESPACE = 'http://www.opengis.net/ows/1.1'
            
            .Object@UTILITY_URL = gsub('process','utility',url)
            
            .Object@emailK      = 'gov.usgs.cida.gdp.wps.algorithm.communication.EmailWhenFinishedAlgorithm'
            
            .Object@url = url
            .Object@algorithm  <- algorithm
            processInputs <- defaultProcessInputs(algorithm = .Object@algorithm[[1]], .Object@url, .Object@WPS_VERSION)
            .Object@processInputs  <-	processInputs
            
            return(.Object)
          })

#' create webprocess object
#'
#' @return the webprocess object
#' @author Jordan S Read
#' @rdname webprocess-methods
#' @export
setGeneric("webprocess", function(...) {
  standardGeneric("webprocess")
})

#'@param ... additional arguments passed initialize method (e.g., \code{url})
#'@rdname webprocess-methods
#'@aliases webprocess
#'@rdname webprocess-methods
setMethod("webprocess", signature(), function(...) {
  ## create new webprocess object
  webprocess <- new("webprocess",...)
  return(webprocess)
})


