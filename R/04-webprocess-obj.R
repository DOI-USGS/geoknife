#' @title webprocess class
#' @description A class representing geoknife web processing specifications
#' @slot url URL for webprocessing service. 
#' Can be set or accessed using \code{\link[geoknife]{url}}
#' @slot algorithm a list for algorithm used. 
#' Can be set or accessed using \code{\link[geoknife]{algorithm}}
#' @slot version a character specifying the wep processing service version to use. 
#' Can be set or accessed using \code{\link[geoknife]{version}}
#' @slot email an email to send finished process alert to
#' @slot wait boolean for wait until complete (hold up R until processing is complete)
#' @slot processInputs (_private) a list of required and options process inputs, and their 
#' default values (if specified). This is populated (or repopulated) whenever \code{algorithm} is set.
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
#' @seealso \code{\link{webprocess}}, \code{\link[geoknife]{url}}, 
#' \code{\link[geoknife]{algorithm}}, \code{\link[geoknife]{version}}
#' @aliases
#' webprocess-class
#' @exportClass webprocess
setClass(
  Class = "webprocess",
  prototype = prototype(
    url = gconfig('wps.url'), 
    algorithm = list('Area Grid Statistics (weighted)'=
                       "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm"),
    version = '1.0.0',
    email = as.character(NA),
    wait = FALSE,
    sleep.time = gconfig('sleep.time')
    ),
  representation = representation(
    url="character",
    algorithm="list",
    version="character",
    email = "character",
    wait = 'logical',
    sleep.time = "numeric",
    processInputs="list",
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
            algorithm = .Object@algorithm,
            version = .Object@version,
            email = .Object@email,
            wait = .Object@wait, 
            sleep.time = .Object@sleep.time, ...)
            {
            .Object@WPS_SCHEMA_LOCATION <- 'http://schemas.opengis.net/wps/1.0.0/wpsExecute_request.xsd'
            .Object@WPS_NAMESPACE <- 'http://www.opengis.net/wps/1.0.0'
            
            .Object@XSI_SCHEMA_LOCATION <- 'http://www.opengis.net/wfs ../wfs/1.1.0/WFS.xsd'
            .Object@XSI_NAMESPACE <- 'http://www.w3.org/2001/XMLSchema-instance'
            
            .Object@OGC_NAMESPACE <- 'http://www.opengis.net/ogc'
            .Object@XLINK_NAMESPACE <- 'http://www.w3.org/1999/xlink'
            
            .Object@OWS_NAMESPACE <- 'http://www.opengis.net/ows/1.1'
            
            .Object@UTILITY_URL <- gsub('process','utility',url)
            
            .Object@emailK <- 'gov.usgs.cida.gdp.wps.algorithm.communication.EmailWhenFinishedAlgorithm'
            
            .Object@version <- version
            .Object@url <- url
            .Object@email <- email
            .Object@wait <- wait
            .Object@sleep.time <- sleep.time
            
            # // -- supporting pass through of existing inputs arguments *when* they are applicable.
            old.inputs = inputs(.Object)
            old.algorithm = .Object@algorithm
            .Object@algorithm <- algorithm
            
            processInputs <- defaultProcessInputs(algorithm = .Object@algorithm[[1]], .Object@url, .Object@version)
            
            if (length(old.inputs) && old.algorithm[[1]] == algorithm[[1]]){
              which.replace <- unlist(unname(lapply(old.inputs, function(x) !is.null(x[[1]]) && !is.na(x[[1]])))) & names(old.inputs) %in% names(processInputs)
              which.to.rplc <- names(processInputs) %in% names(old.inputs[which.replace])
            } else {
              which.to.rplc <- which.replace <- FALSE
            }
            
            .Object@processInputs  <-	processInputs
            .Object@processInputs[which.to.rplc] <- old.inputs[which.replace]
            
            inputs(.Object) <- list(...)
            
            return(.Object)
          })

#' create webprocess object
#'
#' @return the webprocess object
#' @author Jordan S Read
#' @rdname webprocess-methods
#' @export
setGeneric("webprocess", function(.Object, ...) {
  standardGeneric("webprocess")
})

#'@param .Object any object that can be coerced into \linkS4class{webprocess}
#'@param ... additional arguments passed initialize method (e.g., \code{url}, \code{version})
#'@aliases webprocess
#'@rdname webprocess-methods
setMethod("webprocess", signature('missing'), function(.Object, ...) {
  ## create new webprocess object
  webprocess <- new("webprocess",...)
  return(webprocess)
})

#'@aliases webprocess
#'@rdname webprocess-methods
setMethod("webprocess", signature("ANY"), function(.Object,...) {
  ## create new webprocess object
  webprocess <- as(.Object, "webprocess")
  if (!missing(...)){
    webprocess <- initialize(webprocess, ...)
  }
  return(webprocess)
})

