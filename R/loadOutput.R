#'@title parse process output into R environment
#'
#'@details a \code{geoknife} method for loading data into R from a completed processing request
#'
#'@param .Object a \code{\link{geoknife}} object with a successful processID. 
#'(See \code{\link{checkProcess}}).
#'@return list of timeseries values. 
#'@docType methods
#'@keywords methods
#'@import XML
#'@import RCurl
#'@author Jordan S. Read
#'@export
#'@examples
#'\dontrun{
#'gk <- geoknife() # create geoknife object
#'gk # print geoknife object
#'
#'linearRing = bufferPoint(c(-111.48,36.95))
#'setFeature(gk) <-list(LinearRing=linearRing)
#'
#'setAlgorithm(gk) <- getAlgorithms(gk)[4] # feature weighted
#'
#'# set the post inputs for the processing dataset
#'setProcessInputs(gk) <- list('DATASET_ID'='prcp',
#'                             'DATASET_URI'='http://thredds.daac.ornl.gov/thredds/dodsC/daymet-agg/daymet-agg.ncml',
#'                             'TIME_START'='2010-01-01T00:00:00Z',
#'                             'TIME_END'='2010-01-03T00:00:00Z',
#'                             'DELIMITER'='TAB')
#'
#'gk <- startProcess(gk)
#'Sys.sleep(10) # give it some time to process
#'loadOutput(gk) # load and print output
#'}
#'
setGeneric(name="loadOutput",def=function(.Object){standardGeneric("loadOutput")})

#'@aliases loadOutput
setMethod(f = "loadOutput",signature="geoknife",
  definition = function(.Object){
            if (isSuccessful(.Object)){
              output <- outputParse(processID = .Object@processID)
              return(output)
            } else {
              stop('processing is incomplete or has failed. See checkProcess()')
            }
            
          }
  )

outputParse = function(processID){
  funcInfo <- getParseFunction(processID)
  output <- do.call(funcInfo$function_name, args = list('delim' = funcInfo$delim))
  return(output)
}

getParseFunction <- function(processID){
  function_handlers <- list("text/tab-separated-values" = list(function_name = 'parseTimeseries', delim='\t'),
                            "text/csv" = list(function_name = 'parseTimeseries', delim=','))
  # find output type
  doc    <-  htmlParse(processID, isURL=TRUE, useInternalNodes = TRUE)
  type <- xmlGetAttr(getNodeSet(doc,"//reference[@mimetype]")[[1]],'mimetype')
  if (!type %in% names(function_handlers)){
    stop('output ',type, ' not currently supported. Create an issue to suggest it: https://github.com/USGS-R/geoknife/issues/new')
  }
  
  return(function_handlers[[type]])
}

parseTimeseries <- function(file, delim = '\t'){
  return(delim)
}
