#' @title simplegeom class
#' @description The \code{simplegeom} class represents geometries that can 
#' be coerced into polygon features. This is one of two \code{stencil} types 
#' accepted by \code{\link{geoknife}} (the other being \linkS4class{webgeom}).
#' @slot sp a \code{\link[sp]{SpatialPolygons}} object
#' @slot DRAW_NAMESPACE (_private) web location of draw namespace
#' @slot DRAW_SCHEMA (_private) web location of draw schema
#' @details The difference between \linkS4class{webgeom} and \linkS4class{simplegeom} 
#' is both in the permenance and the location of the data. \linkS4class{webgeom} is 
#' located on a web server that offers geometries using the web feature service (WFS) 
#' specification. \linkS4class{simplegeom} are typically local data that can be accessed 
#' within an R session. Within reason, anything that can be represented as a 
#' \linkS4class{webgeom} (or WFS) can also be represented by a \linkS4class{simplegeom} 
#' For example, a state or watershed can be read in as \code{\link[sp]{SpatialPolygons}} 
#' object and turned into a \linkS4class{simplegeom}. 
#' 
#' @rdname simplegeom-class
#'@importClassesFrom sp SpatialPolygons
#'@exportClass simplegeom
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


#' Create simplegeom object
#' 
#' A simple geom is a simple set of geometries specified locally. See 
#' \code{\link{webgeom}} for web features. 
#' 
#' @param .Object any object that can be coerced into \linkS4class{simplegeom}
#' @param ... additional arguments passed to SpatialPolygonsDataFrame
#' @return the simplegeom object
#' @examples 
#' simplegeom(c(-88.6, 45.2))
#' \dontrun{
#' simplegeom(Srl, proj4string = CRS("+proj=longlat +datum=WGS84"))
#' }
#' simplegeom(data.frame('point1'=c(-89, 46), 'point2'=c(-88.6, 45.2)))
#' @author Jordan S Read
#' @rdname simplegeom-methods
#' @export
setGeneric("simplegeom", function(.Object, ...) {
  standardGeneric("simplegeom")
})


#' @rdname simplegeom-methods
#' @aliases simplegeom
setMethod("simplegeom", signature("missing"), function(.Object, ...) {
  ## create new simplegeom object
  # ... are additional arguments passed to SpatialPolygonsDataFrame
  simplegeom <- new("simplegeom",...)
  return(simplegeom)
})

#' @rdname simplegeom-methods
#' @aliases simplegeom
setMethod("simplegeom", signature("ANY"), function(.Object, ...) {
  ## create new simplegeom object
  # ... are additional arguments passed to SpatialPolygonsDataFrame
  simplegeom <- as(.Object, "simplegeom")
  if (!missing(...)){
    simplegeom <- initialize(simplegeom, ...)
  }
  return(simplegeom)
})

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
