#' the attribute of an webgeom object
#'
#' get or set the attribute of a webgeom object. 
#'
#'@param .Object a \linkS4class{webgeom} object
#'@param value a attribute
#'@rdname attribute
#'@aliases
#'attribute
#'attribute<-
#'@export
setGeneric(name="attribute<-",def=function(.Object, value){standardGeneric("attribute<-")})

#'@aliases attribute
#'@rdname attribute
#'@export
setGeneric(name="attribute",def=function(.Object){standardGeneric("attribute")})


#'@aliases attribute
#'@rdname attribute
setMethod(f = "attribute<-",signature(.Object = "webgeom"), definition = function(.Object, value){
  .Object <- initialize(.Object, attribute = value)
  return(.Object)})