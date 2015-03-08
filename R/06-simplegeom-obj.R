#'@importClassesFrom sp SpatialPolygons
#'@export
setClass(
  Class = "simplegeom",
  representation(sp="SpatialPolygons",
                 DRAW_NAMESPACE = "character",
                 DRAW_SCHEMA = "character")
)

#'@importFrom sp SpatialPolygons
setMethod("initialize", signature = "simplegeom", 
          definition = function(.Object, ...) {
            .Object@DRAW_NAMESPACE = 'gov.usgs.cida.gdp.draw'
            .Object@DRAW_SCHEMA = 'http://cida.usgs.gov/climate/derivative/xsd/draw.xsd'
            .Object@sp <- SpatialPolygons(...)
            return(.Object)
})


#'create simplegeom object
#'@description A class representing a simple feature.
#'
#'
#'@return the simplegeom object simple feature
#'@author Jordan S Read
#'@rdname simplegeom-methods
#'@export
setGeneric("simplegeom", function(...) {
  standardGeneric("simplegeom")
})

#'@param ... additional arguments passed to SpatialPolygonsDataFrame
#'@rdname simplegeom-methods
#'@aliases simplegeom,simplegeom-method
setMethod("simplegeom", signature(), function(...) {
  ## create new simplegeom object
  # ... are additional arguments passed to SpatialPolygonsDataFrame
  simplegeom <- new("simplegeom",...)
  return(simplegeom)
})

#'@importFrom sp Polygons Polygon CRS
#'@export
quick_sp <- function(){
  square= rbind(c(-89,42,-88,42,-89,41,-90,41,-89,42),
                c(-79,42,-78,42,-79,41,-80,41,-79,42))
  ID <- c("grid_1", "grid_2")
  Srl = list(
    Polygons(list(Polygon(matrix(square[1, ], ncol=2, byrow=TRUE))), ID[1]),
    Polygons(list(Polygon(matrix(square[2, ], ncol=2, byrow=TRUE))), ID[2])
  )


  sp <- simplegeom(Srl, proj4string = CRS("+proj=longlat +datum=WGS84"))
  return(sp)
}