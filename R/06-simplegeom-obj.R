#'@importClassesFrom sp SpatialPolygons
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
#'@slot DRAW_NAMESPACE location of polygrom draw namespace
#'@slot DRAW_SCHEMA url for draw schema
#'@slot sp object of class \code{\link[sp]{SpatialPolygons}}
#'@return the simplegeom object
#'@author Jordan S Read
#'@rdname simplegeom-methods
#'@export
setGeneric("simplegeom", function(...) {
  standardGeneric("simplegeom")
})

#'@param value a numeric vector (for long,lat point) or data.framed with long, lat pairs, [future support for more]
#'@param ... additional arguments passed to SpatialPolygonsDataFrame
#'@rdname simplegeom-methods
#'@aliases simplegeom,simplegeom-method
#'@examples 
#'as(c(-88.6, 45.2), "simplegeom")
#'\dontrun{
#'simplegeom(Srl, proj4string = CRS("+proj=longlat +datum=WGS84"))
#'}
#'as(data.frame('point1'=c(-89, 46), 'point2'=c(-88.6, 45.2)), "simplegeom")
setMethod("simplegeom", signature(), function(...) {
  ## create new simplegeom object
  # ... are additional arguments passed to SpatialPolygonsDataFrame
  simplegeom <- new("simplegeom",...)
  return(simplegeom)
})

#'@importFrom sp Polygons Polygon CRS
setAs("numeric","simplegeom",function(from) {
 
  ## create new simplegeom object based on a lon lat pair
  if (length(from) == 2){
    ring <- data.frame('bufferedPoint' = from)
  } else {
    stop('numeric input to simplegeom needs to be a single lon,lat pair')
  }
  
  # pass to data.frame method
  return(as(ring, 'simplegeom'))
})

#'@importFrom sp Polygons Polygon CRS
setAs("data.frame", "simplegeom", function(from) {
  
  ## create new simplegeom object based on a lon lat pair
  if (nrow(from) == 2){
    ring <- sapply(from, FUN = bufferPoint)
  } else {
    stop('data.frame input to simplegeom needs to be have 2 rows: longitude & latitude')
  }
  Srl = list()
  for (i in 1:ncol(from)){
    # growing this w/o preallocating ... get sp error w/ rbind
    Srl[[i]] <- Polygons(list(Polygon(matrix(ring[, i], ncol=2, byrow=TRUE))), names(from)[i])
  }
  
  simplegeom <- new("simplegeom", Srl, proj4string = CRS("+proj=longlat +datum=WGS84"))
  return(simplegeom)
})
