
#'@title the url of an object
#'@usage
#'url(.Object)
#'url(.Object) <- 'dods://cida.usgs.gov/thredds/dodsC/prism'
#'@param .Object a webgeom, webdata, geojob, or webprocess object
#'@rdname url
#'@aliases
#'url
#'url<-
#'@export
setGeneric(name="url",def=function(.Object){standardGeneric("url")})

#'@aliases url
#'@export
setGeneric(name="url<-",def=function(.Object, value){standardGeneric("url<-")})

#'@aliases url
#'@export
setMethod(f = "url<-",signature = "webdata", definition = function(.Object, value){
  if (length(value) != 1){
    stop('url must be a single character string')
  }
  .Object@url <- value
  return(.Object)}
)

#'@aliases url
#'@export
setMethod(f = "url<-",signature = "webgeom", definition = function(.Object, value){
  if (length(value) != 1){
    stop('url must be a single character string')
  }
  .Object@url <- value
  return(.Object)}
)

#'@aliases url
#'@export
setMethod(f = "url",signature = "webdata",
          definition = function(.Object){
            return(.Object@url)
          })
#'@aliases url
#'@export
setMethod(f = "url",signature = "webgeom",
          definition = function(.Object){
            return(.Object@url)
          })
#'@aliases url
#'@export
setMethod(f = "url<-",signature = "geojob", definition = function(.Object, value){
  .Object@url <- value
  return(.Object)
}
)
#'@aliases url
#'@export
setMethod(f = "url",signature = "geojob", definition = function(.Object){
  value <- .Object@url
  return(value)
}
)

#'@aliases url
#'@export
setMethod("url<-","webprocess", function(.Object, value){
  .Object@url <- value
  return(.Object)
}
)

#'@aliases url
#'@export
setMethod("url","webprocess", function(.Object){
  value <- .Object@url
  return(value)
}
)