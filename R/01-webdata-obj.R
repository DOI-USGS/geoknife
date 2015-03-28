library(methods)
#' @title webdata class
#' @slot times vector of POSIXct dates (specifying start and end time of processing)
#' @slot url URL of web data
#' @slot variables variable(s) used for processin from dataset
#' @rdname webdata-class
#' @import methods
setClass(
  Class = "webdata",
  prototype = prototype(
    times = as.POSIXct(c(NA,NA)),
    url = as.character(NA),
    variables = as.character(NA)),
  representation = representation(
    times = 'POSIXct',
    url = "character",
    variables = "character",
    dataList = "character",
    timeList = "character")
)


setMethod("initialize", signature = "webdata", 
          definition = function(.Object, times = .Object@times, url = .Object@url, variables = .Object@variables){
            .Object@times = times
            .Object@url = url
            .Object@variables = variables
            .Object@dataList    = 'gov.usgs.cida.gdp.wps.algorithm.discovery.ListOpendapGrids'
            .Object@timeList    = 'gov.usgs.cida.gdp.wps.algorithm.discovery.GetGridTimeRange'
            return(.Object)
          })

#' @title create webdata object
#' @description A class representing a web dataset.
#'
#' @slot times value of type \code{"POSIXct"}, start and stop dates for data
#' @slot url value of type \code{"character"}, the web location for the dataset
#' @slot variable value of type \code{"character"}, the variable(s) for data
#'
#' @return the webdata object representing a dataset and parameters
#' @author Jordan S Read
#' @rdname webdata-methods
#' @docType methods
#' @aliases 
#' webdata
#' @export
setGeneric("webdata", function(.Object,...) {
  standardGeneric("webdata")
})

#'@param .Object any object that can be coerced into \linkS4class{webdata}
#'@param ... additional arguments passed initialize method (e.g., times vector)
#'@rdname webdata-methods
#'@aliases webdata
setMethod("webdata", signature("missing"), function(.Object,...) {
  ## create new webdata object
  webdata <- new("webdata",...)
  return(webdata)
})
#'@rdname webdata-methods
#'@aliases webdata
setMethod("webdata", signature("ANY"), function(.Object,...) {
  ## create new webdata object
  webdata <- as(.Object, "webdata")
  if (!missing(...)){
    webdata <- initialize(webdata, ...)
  }
  return(webdata)
})

setAs("character", "webdata", function(from){
  ## create new webdata object with a character input (for dataset matching)
  if (from != 'prism') stop("character input for webdata not supported for '", from,"'")
  
  webdata <- webdata(times = as.POSIXct(c('1895-01-01 00:00:00','1899-01-01 00:00:00')),
                     url = 'http://cida.usgs.gov/thredds/dodsC/prism',
                     variables = 'ppt')
  return(webdata)
})
