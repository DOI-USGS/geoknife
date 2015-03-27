setClass(
  Class = "geojob",
  representation = representation(
    url = 'character',
    xml = 'character',
    id = "character")
)


setMethod(f="initialize",signature="geojob",
          definition=function(
            .Object, 
            id = '<no active job>',
            url = as.character(NA),
            xml = as.character(NA)){
            
            .Object@xml <- xml
            .Object@id	<- id
            return(.Object)
          })

#' create geojob object
#' @description A class representing a geoknife job (\code{geojob}).
#'
#' @return the geojob object
#' @author Jordan S Read
#' @rdname geojob-methods
#' @export
setGeneric("geojob", function(...) {
  standardGeneric("geojob")
})

#'@param ... additional arguments passed to initialize method
#'@rdname geojob-methods
#'@aliases geojob,geojob-method
setMethod("geojob", signature(), function(...) {
  ## create new geojob object
  geojob <- new("geojob",...)
  return(geojob)
})

#'@rdname geojob-methods
#'@aliases xml<-,geojob-method
#'@param .Object a \code{\link{geojob}} object
#'@param value a character string of xml 
#'@examples
#'xml <- "<foo> <bar> text <baz/> </bar> </foo>"
#'gj <- geojob()
#'xml(gj) <- xml
#'xml(gj)
#'@export
setGeneric(name="xml<-",def=function(.Object, value){standardGeneric("xml<-")})

setMethod(f = "xml<-",signature = "geojob", definition = function(.Object, value){
    .Object@xml <- value
    return(.Object)
  }
)

#'@rdname geojob-methods
#'@aliases xml,geojob-method
#'@examples
#'xml <- "<foo> <bar> text <baz/> </bar> </foo>"
#'gj <- geojob(xml = xml)
#'xml(gj)
#'@export
setGeneric(name="xml",def=function(.Object){standardGeneric("xml")})

setMethod(f = "xml",signature = "geojob", definition = function(.Object){
  value <- .Object@xml
  return(value)
}
)

#'@export
setGeneric(name="id<-",def=function(.Object, value){standardGeneric("id<-")})

setMethod(f = "id<-",signature = "geojob", definition = function(.Object, value){
  .Object@id <- value
  return(.Object)
}
)

#'@export
setGeneric(name="id",def=function(.Object){standardGeneric("id")})

#'@export
setMethod(f = "id",signature = "geojob", definition = function(.Object){
  value <- .Object@id
  return(value)
}
)
