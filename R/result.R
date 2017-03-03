#' parse process output into R environment
#'
#' a \code{geojob} method for loading data into R from a completed processing request
#'
#' @param .Object a \code{\link{geojob}} object with a successful processID,
#' or a \code{character} URL of a completed job.
#' (See \code{\link{check}}).
#' @param ... additional arguments passed to parsers (e.g., with.units = TRUE)
#' @return data.frame of timeseries values. 
#' @rdname result-methods
#' @aliases result
#' @docType methods
#' @keywords methods
#' @importFrom XML xmlGetAttr getNodeSet xmlParse xmlChildren xmlName xpathApply
#' @importFrom httr GET
#' @author Jordan S. Read
#' @export
#' @examples
#' \dontrun{
#' job <- geoknife(stencil = c(-89,42), fabric = 'prism', wait = TRUE)
#' result(job, with.units = TRUE) # load and print output
#' 
#' # or use the job id:
#' id <- id(job)
#' result(id, with.units = TRUE) # load and print output
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
              stop('processing is incomplete or has failed. See check(). Processing status: ',
                   check(.Object)$statusType)
            }
            
          }
  )

#' @rdname result-methods
#' @aliases result
setMethod(f = "result",signature="character",
          definition = function(.Object, ...){
            job <- geojob(id = .Object)
            result(job, ...)
})
outputParse = function(.Object, ...){
  funcInfo <- algorithmParseDetails(.Object)
  fileLocation <- check(.Object)$URL
  output <- do.call(funcInfo[['function.name']], args = list(file = fileLocation, 'delim' = funcInfo[['delimiter']], ...))
  return(output)
}

#' @importFrom utils tail
algorithmParseDetails <- function(job){
  function.handlers <- list("FeatureWeightedGridStatisticsAlgorithm" = c('function.name'='parseTimeseries'),
                            "FeatureGridStatisticsAlgorithm" = c('function.name'='parseTimeseries'),
                            "FeatureCategoricalGridCoverageAlgorithm" = c('function.name'='parseCategorical'))
  if (is.na(xml(job))){
    xmlProcess <- gcontent(gGET(id(job)))
    
    algorithm <- xmlValue(getNodeSet(xmlProcess,"/wps:Process/ows:Identifier")[[1]])
  } else {
    doc <- xmlParse(xml(job))
    algorithm <- xmlValue(getNodeSet(doc,"/wps:Execute/ows:Identifier")[[1]])
    rm(doc) # is this necessary w/ XML package?
  }
  
  algorithm.name <- tail(strsplit(algorithm, '[.]')[[1]], 1)
  
  if (!algorithm.name %in% names(function.handlers)){
    stop('parser for ',algorithm.name, 
         ' not currently supported. Create an issue to suggest it: https://github.com/USGS-R/geoknife/issues/new', call. = FALSE)
  }
  parse.details <- c(function.handlers[[algorithm.name]], 'delimiter'=outputDelimiter(job))
  return(parse.details)
}

outputDelimiter <- function(job){
  delimiters <- c("text/tab-separated-values" = '\t',
                  "text/csv" = ',',
                  'text/plain' = ' ')
  # find output type
  resp <- rawToChar(GET(id(job))$content)
  doc <-  htmlParse(resp, useInternalNodes = TRUE)
  type <- xmlGetAttr(getNodeSet(doc,"//reference[@mimetype]")[[1]],'mimetype')
  if (!type %in% names(delimiters)){
    stop('output ',type, ' not currently supported. Create an issue to suggest it: https://github.com/USGS-R/geoknife/issues/new', call. = FALSE)
  }
  return(delimiters[[type]])
}

