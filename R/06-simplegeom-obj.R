#'@importClassesFrom sp SpatialPolygonsDataFrame
#'@exportClass simplegeom
setClass(
  Class = "simplegeom",
  contains = "SpatialPolygonsDataFrame"
)

# setMethod("initialize", signature = "simplegeom", 
#           definition = function(.Object, polys = as.numeric(NA), IDs = as.character(NA), proj = as.character(NA)){
#             
#             .Object@polys = polys
#             .Object@IDs = IDs
#             .Object@proj = proj
#             return(.Object)
#           })

#' create simplegeom object
#' @description A class representing a simple feature.
#'
#' @slot polys value of type 
#' @slot IDs value of type (same length as polys)
#' @slot proj value of type 
#'
#' @return the simplegeom object simple feature
#' @author Jordan S Read
#' @rdname simplegeom-methods
#' @export

setGeneric("simplegeom", function(...) {
  standardGeneric("simplegeom")
})


#' @rdname simplegeom-methods
#' @aliases simplegeom,simplegeom-method
setMethod("simplegeom", signature(), function(...) {
  ## create new simplegeom object
  simplegeom <- new("simplegeom",...)
  return(simplegeom)
})