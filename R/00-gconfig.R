
pkg.env <- new.env()

pkg.env$urls <- list(geoserver_base = "https://labs.waterdata.usgs.gov/gdp_web/geoserver",
                     process_base = "https://labs.waterdata.usgs.gov/gdp-process-wps/WebProcessingService",
                     csw_url = "https://www.sciencebase.gov/catalog/item/62844f66d34e3bef0c9a48f9/csw",
                     draw_schema = "https://raw.githubusercontent.com/USGS-R/geoknife/main/inst/draw.xsd")

pkg.env$gconfig <- list('wps.url'= pkg.env$urls$process_base,
                        'sleep.time' = 5, 
                        'wait' = FALSE,
                        'email' = as.character(NA),
                        'algorithm' = list('Area Grid Statistics (weighted)' = 
                                             "gov.usgs.cida.gdp.wps.algorithm.FeatureWeightedGridStatisticsAlgorithm"),
                        'verbose' = FALSE,
                        'retries' = 1,
                        'version' = '1.0.0',
                        'show.progress' = TRUE)

pkg.env$NAMESPACES <- c(wps = 'http://www.opengis.net/wps/1.0.0',
                        xsi = 'http://www.w3.org/2001/XMLSchema-instance',
                        xlink = 'http://www.w3.org/1999/xlink',
                        ogc = 'http://www.opengis.net/ogc',
                        ows = 'http://www.opengis.net/ows/1.1',
                        gml = 'http://www.opengis.net/gml',
                        wfs = 'http://www.opengis.net/wfs')

pkg.env$SCHEMA_LOCATIONS <- c(WPS_SCHEMA_LOCATION = 'http://schemas.opengis.net/wps/1.0.0/wpsExecute_request.xsd',
                              XSI_SCHEMA_LOCATION = 'http://www.opengis.net/wfs ../wfs/1.1.0/WFS.xsd',
                              GML_SCHEMA_LOCATION = 'http://schemas.opengis.net/gml/3.1.1/base/feature.xsd')

#' @importFrom utils lsf.str packageName
.onLoad <- function(libname, pkgname){
  setJobState()
  funs <- unclass(lsf.str(envir = asNamespace(packageName()), all = TRUE))
  pkg.env$private.funs <- funs[substr(funs,1,1) == '.']
  rm(funs)
}

library(methods)

#' configure geoknife settings
#' 
#' access and set defaults for geoknife configuration
#' 
#' @param \dots values for gconfig
#' @param no.readonly currently not implemented for \code{TRUE}
#' @return Borrowed text and functionality from \code{\link[graphics]{par}}. 
#' When parameters are set, their previous values are returned in an invisible named list. 
#' Such a list can be passed as an argument to par to restore the parameter values. Use gconfig(no.readonly = TRUE) 
#' for the full list of parameters that can be restored. When just one parameter is queried, the value of that 
#' parameter is returned as (atomic) vector. When two or more parameters are queried, their values are returned 
#' in a list, with the list names giving the parameters. Note the inconsistency: setting one parameter returns a list, 
#' but querying one parameter returns a vector.
#' @export
#' @examples 
#' gconfig # all config
#' gconfig('wait')
#' gconfig('sleep.time' = 10)
#' gconfig('sleep.time' = 8, wait=TRUE)
#' gconfig('progress' = FALSE)
gconfig <- function(..., no.readonly = FALSE){
  
  .gconfig.readonly <- c('version') 
  args <- list(...)
  if (length(names(args) %in% .gconfig.readonly) > 0 && any(names(args) %in% .gconfig.readonly)){
    stop('read only argument(s) ',
         paste(names(args)[names(args) %in% .gconfig.readonly], collapse = ', '), 
         " can't be set", call. = FALSE)
  }
  single <- FALSE
  
  if (length(args) == 0) {
    if (no.readonly) 
      return(pkg.env$gconfig[-match(.gconfig.readonly,names(pkg.env$gconfig))])
    else
      return(pkg.env$gconfig)
  } else {
    if (all(unlist(lapply(args, is.character)))) 
      args <- as.list(unlist(args))
    if (length(args) == 1) {
      if (is.list(args[[1L]]) | is.null(args[[1L]])) 
        args <- args[[1L]]
      else if (is.null(names(args))) 
        single <- TRUE
    }
  }
  
  if (single) 
    value <- pkg.env$gconfig[args[[1L]]][[1L]]
  if (!is.null(names(args))){
    pkg.env$gconfig[c(names(args))] <- args
    invisible(pkg.env$gconfig[c(names(args))])
  }
  else value
  
}