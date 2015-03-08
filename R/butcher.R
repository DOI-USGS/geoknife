

setGeneric(name="butcher",def=function(.Object, x, ...){standardGeneric("butcher")})

setMethod("butcher", signature = c("geojob", "webgeom"), 
          # WILL PROBABLY need to butcher to XML...
          definition = function(.Object, x,  ...)  {
            cat('butcher geojob into webgeom\n')
            return(webgeom())
          }
)
setMethod("butcher", signature = c("character", "webgeom"), 
          definition = function(.Object, x,  ...)  {
            cat('butcher XML into webgeom\n')
            return(webgeom())
          }
)
setMethod("butcher", signature = c("numeric", "webgeom"), 
          definition = function(.Object, x,  ...) {
            # force to be point pair (for now)
            if (length(.Object) != 2) stop('input must be a lat/lon pair')
            ring <- bufferPoint(.Object)
            cat('butcher numeric into linear ring\n');cat(ring);cat('\n')
            return(webgeom())
          }
)
setMethod("butcher", signature = c("SpatialPointsDataFrame", "webgeom"), 
          definition = function(.Object, x,  ...) {
            cat('butcher SpatialPointsDataFrame into webgeom\n')
            return(webgeom())
          }
)
setMethod("butcher", signature = c("SpatialPolygonsDataFrame", "webgeom"), 
          definition = function(.Object, x,  ...) {
            cat('butcher SpatialPolygonsDataFrame into webgeom\n')
            return(webgeom())
          }
)
setMethod("butcher", signature = c("webgeom", "webgeom"), 
          definition = function(.Object, x,  ...) return(.Object) 
)

setMethod("butcher", signature = c("geojob", "webdata"), 
          definition = function(.Object, x,  ...)  {
            cat('butcher geojob into webdata\n')
            return(webdata())
          }
)
setMethod("butcher", signature = c("character", "webdata"), 
          definition = function(.Object, x,  ...) {
            cat('butcher dataset shortname into webdat\n')
            return(webdata())
          }
)
setMethod("butcher", signature = c("webdata", "webdata"), 
          definition = function(.Object, x,  ...) return(.Object) 
)

setMethod("butcher", signature = c("geojob", "webprocess"), 
          definition = function(.Object, x,  ...)  {
            cat('butcher geojob into webprocess\n')
            return(webprocess())
          }
)
setMethod("butcher", signature = c("webprocess", "webprocess"), 
          definition = function(.Object, x,  ...) return(.Object) 
)

missing_msg <- function() stop('stencil and fabric, OR job must be supplied')

