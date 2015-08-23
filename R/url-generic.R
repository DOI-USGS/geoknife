
#'@title the url of an object
#'@usage
#'url(.Object)
#'url(.Object)<- value
#'@param .Object a \linkS4class{webgeom}, \linkS4class{webdata}, 
#'\linkS4class{geojob}, or \linkS4class{webprocess} object
#'@param value a url
#'@rdname url
#'@aliases
#'url
#'url<-
#'@export
setGeneric(name="url<-",def=function(.Object, value){standardGeneric("url<-")})

#'@aliases url
#'@rdname url
#'@export
setGeneric(name="url",def=function(.Object){standardGeneric("url")})


#'@aliases url
#'@rdname url
setMethod(f = "url<-",signature(.Object = "ANY"), definition = function(.Object, value){
  if (length(value) != 1){
    stop('url must be a single character string')
  }
  .Object@url <- value
  return(.Object)})

# special method because other slots depend on value of \code{url} for webprocess object
#'@aliases url
#'@rdname url
setMethod(f = "url<-",signature(.Object = "webprocess"), definition = function(.Object, value){
  if (length(value) != 1){
    stop('url must be a single character string')
  }
  .Object <- new(Class = "webprocess", url = value)
  return(.Object)})

#'@aliases url
#'@rdname url
setMethod(f = "url",signature(.Object = "datagroup"),
          definition = function(.Object){
            return(sapply(.Object@group, function(x) x$url))
          })

#'@aliases url
#'@rdname url
setMethod(f = "url",signature(.Object = "ANY"),
          definition = function(.Object){
            return(.Object@url)
          })
