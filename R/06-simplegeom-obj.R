#'@importClassesFrom sp SpatialPolygonsDataFrame
#'@exportClass simplegeom
setClass(
  Class = "simplegeom",
  contains = "SpatialPolygonsDataFrame"
)

#' create simplegeom object
#' @description A class representing a simple feature.
#'
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