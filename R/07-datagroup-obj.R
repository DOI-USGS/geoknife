#' datagroup class
#' 
#' contains collections of webdata that can be processed with 
#' \code{\link{geoknife}}
#' 
#' @slot group a list of webdata compatable elements
#' @rdname datagroup-class
setClass(
  Class = "datagroup",
  representation = representation(
    group = 'list')
)


setMethod(f="initialize",signature="datagroup",
          definition=function(
            .Object, 
            group = list()){
            
            .Object@group <- group
            return(.Object)
          })

#' create datagroup object
#' @description A class representing a geoknife job (\code{datagroup}).
#'
#' @return the datagroup object
#' @author Jordan S Read
#' @rdname datagroup-methods
#' @export
setGeneric("datagroup", function(...) {
  standardGeneric("datagroup")
})

#' @param x a datagroup object
#' @param i index specifying elements to extract or replace.
#' @param j not implemented
#' @param drop not implemented
#' @param ... additional arguments passed to initialize method
#' @rdname datagroup-methods
#' @aliases datagroup,datagroup-methods
setMethod("datagroup", signature(), function(...) {
  ## create new geojob object
  datagroup <- new("datagroup",...)
  return(datagroup)
})


setAs('datagroup', 'webdata', function(from){
  if (length(from@group) > 1){
    warning('coercing datagroup into webdata. More than one dataset specified, using the first.')
  }
  .Object <- do.call(what = "webdata", args = list(url = from@group[[1]]$url))
  return(.Object)
})


#' get abstract from a datagroup
#' 
#' extracts the abstract information from a datagroup object
#' 
#' @param .Object a datagroup object
#'@rdname abstract-datagroup
#'@aliases 
#'abstract
#'title
#'@export
setGeneric(name="abstract",def=function(.Object){standardGeneric("abstract")})

#'@rdname abstract-datagroup
#'@aliases abstract
setMethod(f = "abstract",signature(.Object = "datagroup"),
          definition = function(.Object){
            return(sapply(.Object@group, function(x) x$abstract))
          })

#' @rdname abstract-datagroup
#' @aliases 
#' abstract
#' title
#'@export
setGeneric(name="title",def=function(.Object){standardGeneric("title")})

#'@rdname abstract-datagroup
#'@aliases 
#'abstract
#'title
setMethod(f = "title",signature(.Object = "datagroup"),
          definition = function(.Object){
            return(sapply(.Object@group, function(x) x$title))
          })
#'@rdname datagroup-methods
#'@aliases datagroup,datagroup-methods
setMethod(f = "length",signature(x = "datagroup"),
          definition = function(x){
            return(length(x@group))
          })


#'@rdname datagroup-methods
#'@aliases datagroup,datagroup-methods
setMethod("[", signature(x='datagroup',i="ANY",j='ANY'), function(x, i, j, ..., drop = TRUE) {
  if (is.character(i))
    i = which(title(x) %in% i)
  return(datagroup(x@group[i]))
})

#'@rdname datagroup-methods
#'@aliases datagroup,datagroup-methods
setMethod("[[", signature('datagroup',i="ANY",j='ANY'), function(x, i, j, ..., drop = TRUE) {
  
  
  return(x@group[[i]])
})