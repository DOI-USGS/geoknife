#' parse process output into R environment
#'
#' a \code{geojob} method for loading data into R from a completed processing request
#'
#' @param .Object a \code{\link{geojob}} object with a successful processID. 
#' (See \code{\link{check}}).
#' @param ... additional arguments passed to parsers (e.g., keep.units = TRUE)
#' @return data.frame of timeseries values. 
#' @rdname result-methods
#' @aliases result
#' @docType methods
#' @keywords methods
#' @importFrom XML xmlGetAttr getNodeSet xmlParse xmlChildren xmlName xpathApply
#' @author Jordan S. Read
#' @export
#' @examples
#' \dontrun{
#' job <- geoknife(stencil = c(-89,42), fabric = 'prism', wait = TRUE)
#' result(job, with.units = TRUE) # load and print output
#' }
#'
setGeneric(name="result",def=function(.Object, ...){standardGeneric("result")})

#' @rdname result-methods
#' @aliases result
setMethod(f = "result",signature="geojob",
  definition = function(.Object, ...){
            if (successful(.Object)){
              output <- outputParse(.Object, ...)
              return(output)
            } else {
              stop('processing is incomplete or has failed. See checkProcess(). Processing status: ',
                   check(.Object)$statusType)
            }
            
          }
  )

outputParse = function(.Object, ...){
  funcInfo <- getParseFunction(id(.Object))
  fileLocation <- check(.Object)$URL
  output <- do.call(funcInfo$function_name, args = list(file = fileLocation, 'delim' = funcInfo$delim, ...))
  return(output)
}

getParseFunction <- function(processID){
  function_handlers <- list("text/tab-separated-values" = list(function_name = 'parseTimeseries', delim='\t'),
                            "text/csv" = list(function_name = 'parseTimeseries', delim=','),
                            'text/plain' = list(function_name = 'parseTimeseries', delim=' '))
  # find output type
  doc <-  htmlParse(processID, isURL=TRUE, useInternalNodes = TRUE)
  type <- xmlGetAttr(getNodeSet(doc,"//reference[@mimetype]")[[1]],'mimetype')
  if (!type %in% names(function_handlers)){
    stop('output ',type, ' not currently supported. Create an issue to suggest it: https://github.com/USGS-R/geoknife/issues/new')
  }
  
  return(function_handlers[[type]])
}

parseCategorical <- function(file, delim){
  stop("function 'parseCategorical' not implemented yet. Create an issue to suggest it: https://github.com/USGS-R/geoknife/issues/new")
}