#'@title the IDs of an object
#'@param .Object a \linkS4class{webgeom} object
#'@param value a IDs
#'@rdname IDs
#'@aliases
#'IDs
#'IDs<-
#'@export
setGeneric(name="IDs<-",def=function(.Object, value){standardGeneric("IDs<-")})

#'@aliases IDs
#'@rdname IDs
#'@export
setGeneric(name="IDs",def=function(.Object){standardGeneric("IDs")})


#'@aliases IDs
#'@rdname IDs
setMethod(f = "IDs<-",signature(.Object = "webgeom"), definition = function(.Object, value){
  .Object <- initialize(.Object, IDs = value)
  return(.Object)})

#'@aliases IDs
#'@rdname IDs
setMethod(f = "IDs",signature="webgeom",
          definition = function(.Object){
            return(.Object@IDs)
          }
)

