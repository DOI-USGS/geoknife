
#' the geom of an object
#' 
#' The "feature" of a webgeom. This is the key mapping to the web resource 
#' that is used as the spatial feature of refernce. Other details specified in 
#' \code{\link{attribute}} and \code{\link{values}}.
#' 
#' @param .Object a \linkS4class{webgeom} object
#' @param value a geom
#' @rdname geom
#' @seealso \code{\link{attribute}} and \code{\link{values}}
#' @aliases
#' geom
#' geom<-
#' @export
setGeneric(name="geom<-",def=function(.Object, value){standardGeneric("geom<-")})

#' @aliases geom
#' @rdname geom
#' @export
setGeneric(name="geom",def=function(.Object){standardGeneric("geom")})


#' @aliases geom
#' @rdname geom
setMethod(f = "geom<-",signature(.Object = "webgeom"), definition = function(.Object, value){
  .Object <- initialize(.Object, geom = value)
  return(.Object)})

#' @rdname geom
#' @aliases geom
setMethod(f = "geom",signature="webgeom",
          definition = function(.Object){
            return(.Object@geom)
          }
)

