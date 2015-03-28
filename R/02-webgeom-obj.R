#' @title webgeom class
#' @slot url URL of web feature service endpoint
#' @slot geom character for geometric feature name
#' @slot attribute character for feature attribute (used for filtering and naming in output)
#' @slot IDs attribute IDs to be used in processing (a subset, or all if NA)
#' @slot wfs_version the web feature service version to use.
#' @slot id job identifier
#' @rdname webgeom-class
setClass(
  Class = "webgeom",
  representation = representation(
    url = "character",
    geom = "character",
    attribute = "character",
    IDs = "character",
    wfs_version = "character",
    WFS_NAMESPACE = "character",
    GML_NAMESPACE = "character",
    GML_SCHEMA_LOCATION = "character"),
)



setMethod("initialize", signature = "webgeom", 
          function(
            .Object,
            url = "http://cida.usgs.gov/gdp/geoserver/wfs",
            geom = as.character(NA), 
            attribute = as.character(NA),
            IDs = as.character(NA), 
            wfs_version = '1.1.0')
            {
            .Object@url= url
            .Object@geom = geom
            .Object@attribute = attribute
            .Object@IDs = IDs
            .Object@wfs_version = wfs_version
            
            .Object@GML_NAMESPACE = 'http://www.opengis.net/gml'
            .Object@WFS_NAMESPACE   = 'http://www.opengis.net/wfs'
            .Object@GML_SCHEMA_LOCATION = 'http://schemas.opengis.net/gml/3.1.1/base/feature.xsd'
            return(.Object)
          })

#' create webgeom object
#' @description A class representing a web dataset.
#'
#' @slot url value of type \code{"character"}, the web location for the dataset
#' @slot variable value of type \code{"character"}, the variable(s) for data
#'
#' @return the webgeom object representing a dataset and parameters
#' @author Jordan S Read
#' @rdname webgeom-methods
#' @export
setGeneric("webgeom", function(...) {
  standardGeneric("webgeom")
})

#' @param ... additional arguments passed initialize method (e.g., \code{url})
#' @rdname webgeom-methods
#' @aliases webgeom
setMethod("webgeom", signature(), function(...) {
  ## create new webgeom object
  webgeom <- new("webgeom", ...)
  return(webgeom)
})




#'@export
quick_wg <- function(){
  wg <- webgeom(geom = "sample:CONUS_states", 
                attribute = "STATE",
                IDs = "CONUS_states.245")
  return(wg)
}
