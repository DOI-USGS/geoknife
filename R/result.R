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
    result(id(.Object), ...)
})

#' @rdname result-methods
#' @aliases result
setMethod(f = "result",signature="character",
          definition = function(.Object, ...){
            if (successful(.Object)){
              output <- outputParse(jobID = .Object, ...)
              return(output)
            } else {
              stop('processing is incomplete or has failed. See check(). Processing status: ',
                   check(.Object)$statusType)
            }
})
outputParse = function(jobID, ...){
  funcInfo <- algorithmParseDetails(jobID)
  fileLocation <- check(jobID)$URL
  output <- do.call(funcInfo[['function.name']], args = list(file = fileLocation, 'delim' = funcInfo[['delimiter']], ...))
  return(output)
}

#' @importFrom utils tail
algorithmParseDetails <- function(jobID){
  function.handlers <- list("FeatureWeightedGridStatisticsAlgorithm" = c('function.name'='parseTimeseries'),
                            "FeatureGridStatisticsAlgorithm" = c('function.name'='parseTimeseries'),
                            "FeatureCategoricalGridCoverageAlgorithm" = c('function.name'='parseCategorical'))
  xmlProcess <- gcontent(gGET(jobID))
  algorithm <- xml2::xml_text(xml2::xml_find_all(xmlProcess,
    "/wps:ExecuteResponse/wps:Process/ows:Identifier")[[1]])

  algorithm.name <- tail(strsplit(algorithm, '[.]')[[1]], 1)
  
  if (!algorithm.name %in% names(function.handlers)){
    stop('parser for ',algorithm.name, 
         ' not currently supported. Create an issue to suggest it: https://github.com/USGS-R/geoknife/issues/new', call. = FALSE)
  }
  parse.details <- c(function.handlers[[algorithm.name]], 'delimiter'=outputDelimiter(jobID))
  return(parse.details)
}

outputDelimiter <- function(jobID){
  delimiters <- c("text/tab-separated-values" = '\t',
                  "text/csv" = ',',
                  'text/plain' = ' ')
  # find output type
  resp <- rawToChar(GET(jobID)$content)
  doc <-  xml2::read_html(resp)
  type <- xml2::xml_attr(xml2::xml_find_all(doc,"//reference[@mimetype]")[[1]],'mimetype')
  if (!type %in% names(delimiters)){
    stop('output ',type, ' not currently supported. Create an issue to suggest it: https://github.com/USGS-R/geoknife/issues/new', call. = FALSE)
  }
  return(delimiters[[type]])
}

