library(methods)
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
setGeneric("webdata", function(value, ...) {
  standardGeneric("webdata")
})

#'@param value character name of dataset or [others supported in the future]
#'@param ... additional arguments passed initialize method (e.g., times vector)
#'@rdname webdata-methods
#'@aliases webdata,webdata-method
setMethod("webdata", signature("missing"), function(value, ...) {
  ## create new webdata object
  webdata <- new("webdata",...)
  return(webdata)
})


setMethod("webdata", signature("character"), function(value, ...) {
  ## create new webdata object with a character input (for dataset matching)
  if (value != 'prism') stop("character input for webdata not supported for '", value,"'")
  
  webdata <- webdata(times = as.POSIXct(c('1895-01-01 00:00:00','1899-01-01 00:00:00')),
                url = 'http://cida.usgs.gov/thredds/dodsC/prism',
                variables = 'ppt')
  webdata <- initialize(webdata, ...) # to pass along additional args
  return(webdata)
})

