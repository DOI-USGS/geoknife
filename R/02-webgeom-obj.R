#' @title webgeom class
#' @description The \code{webgeom} class represents a web feature service (WFS) dataset.
#' WFS is an open geospatial consortium standard for spatial data on the web. WFS supports 
#' filtering of spatial elements and this object can support many of those filters. 
#' @slot url URL of web feature service endpoint. 
#' Can be set or accessed using \code{\link[geoknife]{url}}
#' @slot geom character for geometric feature name. 
#' Can be set or accessed using \code{\link[geoknife]{geom}}
#' @slot attribute character for feature attribute (used for filtering and naming in output)
#' Can be set or accessed using \code{\link[geoknife]{attribute}}
#' @slot values character vector of attribute values to be used in processing (a subset, or all if NA)
#' Can be set or accessed using \code{\link[geoknife]{values}}
#' @slot version a character that specifies the web feature service (WFS) version to use.
#' Can be set or accessed using \code{\link[geoknife]{version}}
#' @slot GML_IDs (_private) IDs that correspond to \code{values}. Used internally for processing. 
#' @slot WFS_NAMESPACE (_private) web location of web feature service namespace
#' @slot GML_NAMESPACE (_private) web location of GML namespace
#' @slot GML_SCHEMA_LOCATION (_private) web location of GML schema location
#' @seealso \code{\link{webgeom}}, \code{\link[geoknife]{url}}, \code{\link[geoknife]{geom}}, 
#' \code{\link[geoknife]{attribute}}, \code{\link[geoknife]{values}}, \code{\link[geoknife]{version}}
#' @rdname webgeom-class
#' @exportClass webgeom
setClass(
  Class = "webgeom",
  prototype = prototype(
    url = "http://cida.usgs.gov/gdp/geoserver/wfs",
    geom = as.character(NA), 
    attribute = as.character(NA),
    values = as.character(NA), 
    version = '1.1.0'
    ),
  representation = representation(
    url = "character",
    geom = "character",
    attribute = "character",
    values = "character",
    version = "character",
    GML_IDs = "character",
    WFS_NAMESPACE = "character",
    GML_NAMESPACE = "character",
    GML_SCHEMA_LOCATION = "character"),
)



setMethod("initialize", signature = "webgeom",
          definition = function(
            .Object, url = .Object@url, geom = .Object@geom, 
            attribute = .Object@attribute,
            values = .Object@values, 
            version = .Object@version
            ){
            .Object@url= url
            .Object@geom = geom
            .Object@attribute = attribute
            .Object@version = version
            .Object@GML_NAMESPACE = 'http://www.opengis.net/gml'
            .Object@WFS_NAMESPACE   = 'http://www.opengis.net/wfs'
            .Object@GML_SCHEMA_LOCATION = 'http://schemas.opengis.net/gml/3.1.1/base/feature.xsd'
            
            values(.Object) = values
            
            
            return(.Object)
          })

#' create webgeom object
#' @description A class representing a web dataset.
#'
#' @slot url value of type \code{"character"}, the web location for the dataset
#' @slot variable value of type \code{"character"}, the variable(s) for data
#'
#' @param .Object any object that can be coerced into \linkS4class{webgeom}
#' @param ... additional arguments passed initialize method (e.g., \code{url})
#' @return the webgeom object representing a dataset and parameters
#' @author Jordan S Read
#' @rdname webgeom-methods
#' @examples
#' wg <- webgeom(geom = "sample:CONUS_states", 
#'  attribute = "STATE",
#'  values = "New Hampshire")
#'#-- use available state datasets:
#' wg <- webgeom('state::NH')
#' wg <- webgeom('state::NH,WI,AL')
#'#-- use available Level III Ecoregion datasets:
#' wg <- webgeom('ecoregion::Colorado Plateaus,Driftless Area')
#'#-- use available simplified HUC8s:
#' wg <- webgeom('HUC8::09020306,14060009')
#' wg <- webgeom()
#' @export
setGeneric("webgeom", function(.Object, ...) {
  standardGeneric("webgeom")
})

#' @rdname webgeom-methods
#' @aliases webgeom
setMethod("webgeom", signature('missing'), function(.Object, ...) {
  ## create new webgeom object
  webgeom <- new("webgeom", ...)
  return(webgeom)
})

#' @rdname webgeom-methods
#' @aliases webgeom
setMethod("webgeom", signature('ANY'), function(.Object, ...) {
  ## create new webgeom object
  webgeom <- as(.Object, "webgeom")
  if (!missing(...)){
    webgeom <- initialize(webgeom, ...)
  }
  return(webgeom)
})

setAs("character", "webgeom", function(from){
  ## create new webdata object with a character input (for dataset matching)
  # supported: two uppercase letters signifying a state abreviation; full state name (first letters capitalized)
  # future support for ecoregions and counties
  
  pieces <- strsplit(from, split = '::')[[1]]
  webgeom <- do.call(paste0('.',pieces[1],'Webgeom'), list(value=pieces[2]))
  
  
  return(webgeom)
})

.stateWebgeom <- function(value){
  multipart <- strsplit(value, split = ',')[[1]]
  values <- unlist(lapply(multipart, function(x) states[[x]]))
  if (length(values) != length(multipart)){
    stop('state matches failed. Check values:',multipart)
  }
  webgeom <- webgeom(geom = "derivative:CONUS_States",
                     attribute = "STATE",
                     values = values)
  return(webgeom)
}

.ecoregionWebgeom <- function(value){
  multipart <- strsplit(value, split = ',')[[1]]
  values <- multipart
  webgeom <- webgeom(geom = "derivative:Level_III_Ecoregions",
                     attribute = "LEVEL3_NAM",
                     values = values)
  return(webgeom)
}

.HUC8Webgeom <- function(value){
  multipart <- strsplit(value, split = ',')[[1]]
  values <- multipart
  webgeom <- webgeom(geom = "derivative:wbdhu8_alb_simp",
                     attribute = "HUC_8",
                     values = values)
  return(webgeom)
}

