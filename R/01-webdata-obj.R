library(methods)
setClass(
  Class = "webdata",
  representation = representation(
    times = 'POSIXct',
    url = "character",
    variables = "character",
    dataList = "character",
    timeList = "character")
)
setMethod("initialize", signature = "webdata", 
          definition = function(.Object, times = as.POSIXct(c(NA,NA)), url = as.character(NA), variables = as.character(NA)){
            
            .Object@times = times
            .Object@url = url
            .Object@variables = variables
            .Object@dataList    = 'gov.usgs.cida.gdp.wps.algorithm.discovery.ListOpendapGrids'
            .Object@timeList    = 'gov.usgs.cida.gdp.wps.algorithm.discovery.GetGridTimeRange'
            return(.Object)
          })

#' create webdata object
#' @description A class representing a web dataset.
#'
#' @slot times value of type \code{"POSIXct"}, start and stop dates for data
#' @slot url value of type \code{"character"}, the web location for the dataset
#' @slot variable value of type \code{"character"}, the variable(s) for data
#'
#' @return the webdata object representing a dataset and parameters
#' @author Jordan S Read
#' @rdname webdata-methods
#' @export
setGeneric("webdata", function(...) {
  standardGeneric("webdata")
})

#'@param ... additional arguments passed initialize method (e.g., times vector)
#'@rdname webdata-methods
#'@aliases webdata,webdata-method
setMethod("webdata", signature(), function(...) {
  ## create new webdata object
  webdata <- new("webdata",...)
  return(webdata)
})

#'@rdname webdata-methods
#'@aliases times<-,webdata-method
#'@export
setGeneric(name="times<-",def=function(.Object, value){standardGeneric("times<-")})

#'@param .Object a \code{\link{webdata}} object
#'@param value a POSIXct vector
#'@examples
#'wd <- webdata()
#'times(wd) <- as.POSIXct(c("2012-11-04", "2012-11-12"))
#'times(wd)[1] <- as.POSIXct("2012-11-04")
#'@export
#'@rdname webdata-methods
#'@aliases times<-,webdata-method
setMethod(f = "times<-",signature = "webdata",
                 definition = function(.Object, value){
                   if (length(value) != 2){
                     stop('times input must be a POSIXct vector of length 2')
                   }
                   .Object@times <- value
                   
                   if (!any(is.na(value)) && value[1] >= value[2]){
                     stop('time start must proceed time stop in "times" slot for webdata')
                   }
                   return(.Object)
})

#'@rdname webdata-methods
#'@aliases times,webdata-method
#'@export
setGeneric(name="times",def=function(.Object){standardGeneric("times")})

setMethod(f = "times",signature = "webdata",
                 definition = function(.Object){
                   return(.Object@times)
                 })



#'@rdname webdata-methods
#'@aliases variables,webdata-method
#'@export
setGeneric(name="variables",def=function(.Object){standardGeneric("variables")})

#'@param .Object a \code{\link{webdata}} object
#'@rdname webdata-methods
#'@aliases variables,webdata-method
#'@export
setMethod(f = "variables",signature = "webdata",
          definition = function(.Object){
            return(.Object@variables)
          })

#'@rdname webdata-methods
#'@aliases variables<-,webdata-method
#'@export
setGeneric(name="variables<-",def=function(.Object, value){standardGeneric("variables<-")})

#'@param .Object a \code{\link{webdata}} object
#'@param value a character vector for variables
#'@rdname webdata-methods
#'@aliases variables,webdata-method
#'@export
setMethod(f = "variables<-",signature = "webdata",
          definition = function(.Object, value){
            .Object@variables <- value
            return(.Object)
          })


#'@export
quick_wd <- function(){
  wd <- webdata(times = as.POSIXct(c('1895-01-01 00:00:00','1899-01-01 00:00:00')),
                url = 'http://cida.usgs.gov/thredds/dodsC/prism',
                variables = 'ppt')
  return(wd)
}