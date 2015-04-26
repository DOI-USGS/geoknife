#'@title the values of an object
#'@param .Object a \linkS4class{webgeom} object
#'@param value a values
#'@rdname values
#'@aliases
#'values
#'values<-
#'@export
setGeneric(name="values<-",def=function(.Object, value){standardGeneric("values<-")})

#'@aliases values
#'@rdname values
#'@export
setGeneric(name="values",def=function(.Object){standardGeneric("values")})


#'@aliases values
#'@rdname values
setMethod(f = "values<-",signature(.Object = "webgeom"), definition = function(.Object, value){
  .Object@values <- value
  .Object@GML_IDs <- "CONUS_States.28"
  return(.Object)})

#'@aliases values
#'@rdname values
setMethod(f = "values",signature="webgeom",
          definition = function(.Object){
            return(.Object@values)
          }
)

