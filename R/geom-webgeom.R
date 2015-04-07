
#'@title the geom of an object
#'@param .Object a \linkS4class{webgeom} object
#'@param value a geom
#'@rdname geom
#'@aliases
#'geom
#'geom<-
#'@export
setGeneric(name="geom<-",def=function(.Object, value){standardGeneric("geom<-")})

#'@aliases geom
#'@rdname geom
#'@export
setGeneric(name="geom",def=function(.Object){standardGeneric("geom")})


#'@aliases geom
#'@rdname geom
setMethod(f = "geom<-",signature(.Object = "webgeom"), definition = function(.Object, value){
  .Object <- initialize(.Object, geom = value)
  return(.Object)})