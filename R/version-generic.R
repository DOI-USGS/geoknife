#' the version of an object
#' 
#' get the version of webgeom or webprocess
#' 
#' @param .Object a \linkS4class{webgeom} or \linkS4class{webprocess} object
#' @param value a version
#' @rdname version
#' @aliases
#' version
#' version<-
#' @export
setGeneric(name="version<-",def=function(.Object, value){standardGeneric("version<-")})

#' @aliases version
#' @rdname version
#' @export
setGeneric(name="version",def=function(.Object){standardGeneric("version")})


#' @aliases version
#' @rdname version
setMethod(f = "version<-",signature(.Object = "ANY"), definition = function(.Object, value){
  .Object <- initialize(.Object, version = value)
  return(.Object)})

#' @aliases version
#' @rdname version
setMethod(f = "version",signature="ANY",
          definition = function(.Object){
            return(.Object@version)
          }
)

