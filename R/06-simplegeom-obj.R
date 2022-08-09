setOldClass("sf")

#' @title simplegeom class
#' @description The \code{simplegeom} class represents geometries that can 
#' be coerced into polygon features. This is one of two \code{stencil} types 
#' accepted by \code{\link{geoknife}} (the other being \linkS4class{webgeom}).
#' @slot sf an sf data.frame object with polygon geometries
#' @slot sp an sp object provided for backward compatibility
#' @slot DRAW_NAMESPACE (_private) web location of draw namespace
#' @slot DRAW_SCHEMA (_private) web location of draw schema
#' @details The difference between \linkS4class{webgeom} and \linkS4class{simplegeom} 
#' is both in the permanence and the location of the data. \linkS4class{webgeom} is 
#' located on a web server that offers geometries using the web feature service (WFS) 
#' specification. \linkS4class{simplegeom} are typically local data that can be accessed 
#' within an R session. Within reason, anything that can be represented as a 
#' \linkS4class{webgeom} (or WFS) can also be represented by a \linkS4class{simplegeom} 
#' For example, a state or watershed can be read in as \code{\link[sf]{read_sf}} 
#' object and turned into a \linkS4class{simplegeom}. IDs of a web geom are the
#' row order of the geometries.
#' 
#' @rdname simplegeom-class
#' @exportClass simplegeom
setClass(
  Class = "simplegeom",
  representation(sf= "sf",
                 sp = "ANY",
                 DRAW_NAMESPACE = "character",
                 DRAW_SCHEMA = "character")
)

#'@importFrom sf st_sf as_Spatial
setMethod("initialize", signature = "simplegeom", 
          definition = function(.Object, ...) {
            .Object@DRAW_NAMESPACE = 'gov.usgs.cida.gdp.draw'
            .Object@DRAW_SCHEMA = "https://cida.usgs.gov/gdp/geoserver/www/draw.xsd"
            # FIXME switch back when draw schema is available via labs
            # .Object@DRAW_SCHEMA = paste0(pkg.env$urls$geoserver_base, 
            #                              '/www/draw.xsd')
            .Object@sf = st_sf(...)
            .Object@sp = as_Spatial(.Object@sf)
            return(.Object)
          })


#' Create simplegeom object
#' 
#' A simple geom is a simple set of geometries specified locally. See 
#' \code{\link{webgeom}} for web features. 
#' 
#' @param .Object any object that can be coerced into \linkS4class{simplegeom}
#' @param ... additional arguments passed to \code{\link[sf]{st_sf}}
#' @return the simplegeom object
#' @examples 
#' 
#' simplegeom(c(-88.6, 45.2))
#' 
#' p1 <- sf::st_polygon(list(cbind(c(-89.0001,-89,-88.9999,-89,-89.0001),
#'                                 c(46,46.0001,46,45.9999,46))))
#' 
#' p2 <- sf::st_polygon(list(cbind(c(-88.6,-88.5999,-88.5999,-88.6,-88.6),
#'                                 c(45.2,45.2,45.1999,45.1999,45.2))))
#' 
#' P <- simplegeom(
#'   sf::st_sf(geo = sf::st_sfc(list(p1, p2), crs = 4326))
#' )
#' 
#' \dontrun{
#' result(geoknife(P, "prism", wait = TRUE))
#' }
#' 
#' simplegeom(data.frame('point1'=c(-89, 46), 'point2'=c(-88.6, 45.2)))
#' 
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
  
  simplegeom <- tryCatch({
    
    new("simplegeom", ...)
    
  }, error = function(e) {
    
    warning("SpatialPolygons support is deprecated.")
    
    if(!requireNamespace("sp")) stop("sp required for spatialpolygons support")
    
    as(sp::SpatialPolygons(...), "simplegeom")
    
  })
  
  return(simplegeom)
})

#' @rdname simplegeom-methods
#' @aliases simplegeom
setMethod("simplegeom", signature("ANY"), function(.Object, ...) {
  ## create new simplegeom object
  # ... are additional arguments passed to sf::st_sf
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

#'@importFrom sf st_polygon st_sfc 
setAs("data.frame", "simplegeom", function(from) {
  
  ## create new simplegeom object based on a lon lat pair
  if (nrow(from) == 2){
    ring <- sapply(from, FUN = bufferPoint)
  } else {
    stop('data.frame input to simplegeom needs to be have 2 rows: longitude & latitude')
  }
  
  poly <- apply(ring, 2, function(x) {
    sf::st_polygon(list(matrix(x, ncol = 2, byrow = TRUE)))
  })
  
  poly <- sf::st_sf(ID = names(poly), geom = sf::st_sfc(poly, crs = 4326))
  
  simplegeom <- as(poly, "simplegeom")
  
  return(simplegeom)
})

#' @importFrom sf st_crs st_transform st_geometry_type
setAs("sf", "simplegeom", function(from) {
  
  if(is.na(sf::st_crs(from))) {
    stop("No CRS provided to simplegeom.")
  }
  
  if (sf::st_crs(from) != sf::st_crs(4326)) {
    warning("CRS of input not WGS84 lon/lat -- transformation is being applied")
    
    from <- sf::st_transform(from, 4326)
    
  }  
  
  if(!grepl("polygon", st_geometry_type(from, by_geometry = FALSE), 
            ignore.case = TRUE)) {
    stop("sf data.frame must contain polygon or multipolygon geometries.")
  }
  
  simplegeom <- new("simplegeom", from)
  
  return(simplegeom)
  
})

setAs("SpatialPolygons", "simplegeom", function(from) {
  
  from <- as(sf::st_as_sf(from), "simplegeom")
  
  return(from)
})

setMethod("show", "simplegeom", function(object){
  cat('An object of class "simplegeom":\n')
  print(object@sf)
})