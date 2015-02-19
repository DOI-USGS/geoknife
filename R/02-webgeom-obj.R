setUrl <- function(.Object, values){
  if (length(values) != 1){
    stop('url must be a single character string')
  }
  .Object@url <- values

  return(.Object)
}

setClass(
  Class = "webgeom",
  representation = representation(
    url = "character",
    geom = "character",
    attribute = "character",
    IDs = "character")
)


setMethod("initialize", signature = "webgeom", 
          function(
            .Object,
            url, geom = as.character(NA), 
            attribute = as.character(NA), IDs = as.character(NA))
            {
              
            if (missing(url)){
              url = "http://cida.usgs.gov/gdp/geoserver/wfs"
            }
            
            .Object@url= url
            .Object@geom = geom
            .Object@attribute = attribute
            .Object@IDs = IDs
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

#'@rdname webgeom-methods
#'@aliases url<-,webgeom-method
#'@export
setGeneric(name="url<-",def=function(.Object, values){standardGeneric("url<-")})

setMethod(f = "url<-",signature = "webgeom", definition = function(.Object, values){
  setURL(.Object, values)}
  )


#' @rdname webgeom-methods
#' @aliases webgeom,webgeom-method
setMethod("webgeom", signature(), function(...) {
  ## create new webgeom object
  webgeom <- new("webgeom",...)
  return(webgeom)
})
