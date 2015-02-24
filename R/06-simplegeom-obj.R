#'@importClassesFrom sp SpatialPolygonsDataFrame
#'@exportClass SpatialPolygonsDataFrame
#'@export
setClass(
  Class = "simplegeom",
  representation(sp="SpatialPolygons")
)


setMethod("initialize", signature = "simplegeom", 
          definition = function(.Object, ...) {
            .Object@sp <- SpatialPolygons(...)
            return(.Object)
})


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
  # ... are additional arguments passed to SpatialPolygonsDataFrame
  simplegeom <- new("simplegeom",...)
  return(simplegeom)
})

#'@export
quick_sp <- function(){
  Sr1 = Polygon(cbind(c(-89,-88,-89,-90,-89),c(42,42,41,41,42)))
  
  Srs1 = Polygons(list(Sr1), "s1")
  sp <- simplegeom(list(Srs1), proj4string = CRS("+proj=longlat +datum=WGS84"))
  return(sp)
}