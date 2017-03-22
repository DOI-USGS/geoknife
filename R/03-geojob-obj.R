#' geojob class
#' 
#' contains the information for processing the job, and the versions 
#' of the resources used. 
#' 
#' @slot url URL of web processing endpoint
#' @slot xml XML character for post
#' @slot id job identifier
#' @slot package.version the version of the geoknife package
#' @slot algorithm.version the version of the algorithm used for processing
#' @importFrom utils packageVersion
#' @rdname geojob-class
setClass(
  Class = "geojob",
  representation = representation(
    url = 'character',
    xml = 'character',
    package.version = 'character',
    algorithm.version = 'character',
    id = "character")
)


setMethod(f="initialize",signature="geojob",
          definition=function(
            .Object, 
            id = '<no active job>',
            url = as.character(NA),
            algorithm.version = as.character(NULL),
            xml = as.character(NA)
            ){
            
            .Object@xml <- xml
            .Object@id	<- id
            .Object@url <- url
            .Object@package.version = as.character(package_version(packageVersion(getPackageName())))
            .Object@algorithm.version = algorithm.version
            return(.Object)
          })

#' create geojob object
#' @description A class representing a geoknife job (\code{geojob}).
#'
#' @return the geojob object
#' @author Jordan S Read
#' @rdname geojob-methods
#' @export
setGeneric("geojob", function(xml, ...) {
  standardGeneric("geojob")
})

#'@param ... additional arguments passed to initialize method
#'@rdname geojob-methods
#'@aliases geojob,geojob-method
setMethod("geojob", signature("missing"), function(xml, ...) {
  ## create new geojob object
  geojob <- new("geojob",...)
  return(geojob)
})

#' @importFrom XML toString.XMLNode xmlAttrs xmlRoot
#' @rdname geojob-methods
#' @aliases geojob,geojob-method
setMethod("geojob", signature("XMLDocument"), function(xml, ...) {
  #slots
  browser()
  xmlText <- toString.XMLNode(xml$doc$children[[1]])
  algorithm.version <- xmlAttrs(xmlRoot(xml))[['version']] #TODO: with XML not xml2
  
  job <- new("geojob", xml = xmlText, 
             algorithm.version = algorithm.version, ...)
  return(job)
})


#'@param xml location of xml (URL or local path) 
#'@rdname geojob-methods
#'@importFrom RCurl url.exists
#'@importFrom XML xmlTreeParse 
#'@aliases geojob,geojob-method
setMethod("geojob", signature("character"), function(xml, ...) {
  #parse based on xml class
  if(url.exists(xml)){
    xml <- gGET(xml)
  } 
  doc <- xmlTreeParse(xml)
  job <- geojob(xml = doc, ...) 
  return(job)
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

#'@title process id of geojob
#'@rdname geojob-methods
#'@aliases id,geojob-method
#'@usage
#'id(.Object)
#'id(.Object) <- value
#'@examples
#'id(gj)
#'@export
setGeneric(name="id<-",def=function(.Object, value){standardGeneric("id<-")})

#'@rdname geojob-methods
#'@aliases id,geojob-method
#'@export
setGeneric(name="id",def=function(.Object){standardGeneric("id")})

#'@rdname geojob-methods
#'@aliases id,geojob-method
setMethod(f = "id<-",signature = "geojob", definition = function(.Object, value){
  .Object@id <- value
  return(.Object)
})

#'@rdname geojob-methods
#'@aliases id,geojob-method
setMethod(f = "id",signature = "geojob", definition = function(.Object){
  value <- .Object@id
  return(value)
})

#'@rdname geojob-methods
#'@aliases id,geojob-method
setMethod(f = "id",signature = "character", definition = function(.Object){
  if (is.geojobID(.Object)){
    return(.Object)  
  } else {
    stop(.Object, ' is not a valid geojob id')
  }
  
})

is.geojobID <- function(id){
  grepl('?id=', id)
}