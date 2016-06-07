pkg.env <- new.env()
pkg.env$gconfig <- list('wps.url'='http://cida.usgs.gov/gdp/process/WebProcessingService',
                        'sleep.time' = 5, 
                        'wait' = FALSE,
                        'email' = as.character(NA),
                        'algorithm' = list('Area Grid Statistics (weighted)' = 
                                             "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm"),
                        'verbose' = FALSE,
                        'retries' = 1,
                        'version' = '1.0.0')

#' @importFrom utils lsf.str packageName
.onLoad <- function(libname, pkgname){
  setJobState()
  funs <- unclass(lsf.str(envir = asNamespace(packageName()), all = TRUE))
  pkg.env$private.funs <- funs[substr(funs,1,1) == '.']
  rm(funs)
}



library(methods)

#' 
#' webdata class
#' 
#' a class for specifying details of web datasets (webdata!). These datasets have to be  
#' accessible through the OPeNDAP protocol or as WCS (web coverage services). 
#' 
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
            .Object@times <- geotime(times)
            .Object@url <- url
            .Object@variables <- variables
            .Object@dataList <- 'gov.usgs.cida.gdp.wps.algorithm.discovery.ListOpendapGrids'
            .Object@timeList <- 'gov.usgs.cida.gdp.wps.algorithm.discovery.GetGridTimeRange'
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
#'  (currently \code{character}, \code{webdata}, and \code{list})
#'@param ... additional arguments passed initialize method (e.g., times, or any other 
#'in the \linkS4class{webdata} object. 
#'@examples
#'webdata('prism')
#'webdata('prism', times=as.POSIXct(c('1990-01-01', '1995-01-01')))
#'webdata(list(times = as.POSIXct(c('1895-01-01 00:00:00','1899-01-01 00:00:00')),
#'  url = 'http://cida.usgs.gov/thredds/dodsC/prism',
#'  variables = 'ppt'))
#'@rdname webdata-methods
#'@aliases webdata
setMethod("webdata", signature("missing"), function(.Object,...) {
  ## create new webdata object
  webdata <- new("webdata",...)
  return(webdata)
})
#'@rdname webdata-methods
#'@aliases webdata
setMethod("webdata", signature("character"), function(.Object=c("prism",  "iclus",  "daymet", "gldas",  "nldas",  "topowx", "solar",  "metobs"),...) {
  ## create new webdata object
  webdata <- as(.Object, "webdata")
  if (!missing(...)){
    webdata <- initialize(webdata, ...)
  }
  return(webdata)
})

#'@rdname webdata-methods
#'@aliases webdata
setMethod("webdata", signature("ANY"), function(.Object, ...) {
  ## create new webdata object
  webdata <- as(.Object, "webdata")
  if (!missing(...)){
    webdata <- initialize(webdata, ...)
  }
  return(webdata)
})

getPkgData <- function(){
  list('prism' = 
         list(times = as.POSIXct(c('1895-01-01 00:00:00','1899-01-01 00:00:00')),
              url = 'http://cida.usgs.gov/thredds/dodsC/prism_v2',
              variables = 'ppt'),
       'iclus' = 
         list(url = 'http://cida.usgs.gov/thredds/dodsC/iclus/hc',
              variables = 'housing_classes_iclus_a1_2010'),
       'daymet' = 
         list(times = as.POSIXct(c('2000-01-01 00:00:00','2001-01-01 00:00:00')),
              url = 'http://thredds.daac.ornl.gov/thredds/dodsC/daymet-agg/daymet-agg.ncml',
              variables = 'prcp'),
       'gldas' = 
         list(times = as.POSIXct(c('2010-01-01 00:00:00','2010-01-02 00:00:00')),
              url = 'dods://hydro1.sci.gsfc.nasa.gov/dods/GLDAS_NOAH10_3H.020',
              variables = 'avgsurftsfc'),
       'nldas' = 
         list(times = as.POSIXct(c('2010-01-01 00:00:00','2010-01-02 00:00:00')),
              url = 'dods://hydro1.sci.gsfc.nasa.gov/dods/NLDAS_FORA0125_H.002',
              variables = 'tmp2m'),
       'topowx' = 
         list(times = as.POSIXct(c('2010-01-01 00:00:00','2010-02-01 00:00:00')),
              url = 'http://cida.usgs.gov/thredds/dodsC/topowx',
              variables = 'tmax'),
       'solar' = 
         list(times = as.POSIXct(c('1980-01-01 00:00:00','1980-12-01 00:00:00')),
              url = 'http://cida.usgs.gov/thredds/dodsC/mows/sr',
              variables = 'sr'),
       'metobs' = 
         list(times = as.POSIXct(c('2010-01-01 00:00:00','2010-01-02 00:00:00')),
              url = 'http://cida.usgs.gov/thredds/dodsC/new_gmo',
              variables = 'tas'))
}

setAs("character", "webdata", function(from){
  ## create new webdata object with a character input (for dataset matching)
  
  datasets <- getPkgData()
  
  from <- match.arg(arg = from, names(datasets))
  
  .Object<- do.call(webdata, args = datasets[[from]])
  return(.Object)
})

setAs('list', 'webdata', function(from){
  
  .Object <- do.call(what = "webdata", args = from)
  return(.Object)
})

