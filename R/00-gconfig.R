
#' congfigure geoknife settings
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