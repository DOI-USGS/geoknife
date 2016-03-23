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
#' @slot sleep.time numeric for time to wait in between calls to \code{\link{check}}. Only used if \code{wait=TRUE}
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
    version = gconfig('version'),
    WPS_SCHEMA_LOCATION = 'http://schemas.opengis.net/wps/1.0.0/wpsExecute_request.xsd',
    WPS_NAMESPACE = 'http://www.opengis.net/wps/1.0.0',
    XSI_SCHEMA_LOCATION = 'http://www.opengis.net/wfs ../wfs/1.1.0/WFS.xsd',
    XSI_NAMESPACE = 'http://www.w3.org/2001/XMLSchema-instance',
    OGC_NAMESPACE = 'http://www.opengis.net/ogc',
    XLINK_NAMESPACE = 'http://www.w3.org/1999/xlink',
    OWS_NAMESPACE = 'http://www.opengis.net/ows/1.1',
    emailK = 'gov.usgs.cida.gdp.wps.algorithm.communication.EmailWhenFinishedAlgorithm'
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
          
            # // -- supporting pass through of existing inputs arguments *when* they are applicable.
            old.algorithm <- if(length(.Object@algorithm) > 0) .Object@algorithm else gconfig('algorithm')
            old.inputs = inputs(.Object)
            
            #things that use package variables:
            .Object@url <- if(length(url) > 0) url else gconfig('wps.url')
            .Object@sleep.time <- if(length(sleep.time) > 0) sleep.time else gconfig('sleep.time')
            .Object@wait <- if(length(wait) > 0) wait else gconfig('wait')
            .Object@email <- if(length(email) > 0) email else gconfig('email')
            .Object@algorithm <- if(length(algorithm) > 0) algorithm else gconfig('algorithm') # will need to do this differently...
            
            .Object@UTILITY_URL <- gsub('process','utility', .Object@url)
            
            processInputs <- defaultProcessInputs(algorithm = .Object@algorithm[[1]], .Object@url, .Object@version)
            
            if (length(old.inputs) > 0 && old.algorithm[[1]] == algorithm[[1]]){
              which.replace <- unlist(unname(lapply(old.inputs, function(x) !is.null(x[[1]]) && !is.na(x[[1]])))) & names(old.inputs) %in% names(processInputs)
              which.to.rplc <- names(processInputs) %in% names(old.inputs[which.replace])
            } else {
              which.to.rplc <- which.replace <- FALSE
            }
            
            .Object@processInputs  <-	processInputs
            .Object@processInputs[which.to.rplc] <- old.inputs[which.replace]
            
            other.inputs <- list(...)
            if (any(paste0('.',names(other.inputs)) %in% pkg.env$private.funs))
              stop('read only variables in webprocess cannot be set', call. = FALSE)
            inputs(.Object) <- other.inputs
            
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
setMethod("webprocess", signature("chracter"), function(.Object=c("summary","unweighted summary","coverage summary","subset","coverage subset" ), ...) {
  ## create new webprocess object
  webprocess <- as(.Object, "webprocess")
  if (!missing(...)){
    webprocess <- initialize(webprocess, ...)
  }
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

getKnives <- function(){
  list('summary' =
         list(algorithm=
                list('Area Grid Statistics (weighted)' =
                       "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm")),
       'unweighted summary' =
         list(algorithm=
                list('Area Grid Statistics (unweighted)' =
                       "gov.usgs.cida.gdp.wps.algorithm.FeatureGridStatisticsAlgorithm")),
       'coverage summary' = 
         list(algorithm=
                list('Categorical Coverage Fraction'=
                       "gov.usgs.cida.gdp.wps.algorithm.FeatureCategoricalGridCoverageAlgorithm")),
       'subset' = 
         list(algorithm=
                list('OPeNDAP Subset' =
                       "gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm")),
       'coverage subset' = 
         list(algorithm=
                list('WCS Subset' = 
                       "gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageIntersectionAlgorithm")))
}
setAs("character", "webprocess", function(from){
  ## create new webdata object with a character input (for dataset matching)
  knives <- getKnives()
                   
  from <- match.arg(arg = from, names(knives))
  
  .Object<- do.call(webprocess, args = knives[[from]])
  return(.Object)
})

