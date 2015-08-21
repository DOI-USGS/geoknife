#' @title datagroup class
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

#'@param ... additional arguments passed to initialize method
#'@rdname datagroup-methods
#'@aliases datagroup,datagroup-methods
setMethod("datagroup", signature(), function(...) {
  ## create new geojob object
  datagroup <- new("datagroup",...)
  return(datagroup)
})

#'@rdname datagroup-methods
#'@aliases datagroup,datagroup-methods
setMethod("[", signature('datagroup'), function(x, i, j, ..., drop = TRUE) {
  
  
  return(datagroup(x@group[i]))
})

#'@rdname datagroup-methods
#'@aliases datagroup,datagroup-methods
setMethod("[[", signature('datagroup'), function(x, i, j, ..., drop = TRUE) {
  
  
  return(x@group[[i]])
})