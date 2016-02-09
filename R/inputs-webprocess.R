#' inputs of webprocess
#' 
#' The webprocess inputs are a list of parameter names and their associated values.  
#' These inputs are specific to an individual \code{algorithm} used by the webprocess, 
#' and are pulled in from the web with defaults. An NA value in an input field means that 
#' this parameter will be left out of the web processing request. NULL values are required fields 
#' for the post and must be populated. For some values, there are defaults used if NULL (e.g.,
#' the mean for 'STATISTIC' is "mean"). Only characters can be used in input fields, but 
#' booleans are changed to characters as 'true' and 'false'. 
#' 
#' @param .Object a webprocessing object
#' @param \dots arguments matching fields in .Object's processInputs slot
#' @param value a field pair for .Object processInputs
#' @rdname inputs-webprocess
#'@aliases
#'inputs<-
#'@examples
#'wp <- webprocess(DELIMITER = 'TAB')
#'inputs(wp)
#'
#'inputs(wp) <- list(DELIMITER = 'COMMA', SUMMARIZE_FEATURE_ATTRIBUTE = 'false')
#'inputs(wp)
#'inputs(wp, "DELIMITER")
#'inputs(wp, "DELIMITER") <- "TAB"
#'@export
#'@keywords internal
setGeneric(name="inputs<-",def=function(.Object, ..., value){
  standardGeneric("inputs<-")
})

#'@title inputs of webprocess
#'@description access or set the inputs of a webprocess
#'@param .Object a webprocessing object
#'@param ... arguments matching fields in .Object's processInputs slot
#'@rdname inputs-webprocess
#'@aliases inputs
#'@export
#'@keywords internal
setGeneric(name="inputs",def=function(.Object, ..., value){standardGeneric("inputs")})

#'@rdname inputs-webprocess
#'@aliases inputs
setMethod(f = "inputs",signature = "webprocess",
          definition = function(.Object, ..., value){
            if (missing(...)){
              return(.Object@processInputs)
            } else {
              ret_idx <- which(names(.Object@processInputs) %in% as.character(expand.grid(...,stringsAsFactors = F)))
              return(.Object@processInputs[ret_idx])
            }
          })


#'@rdname inputs-webprocess
#'@aliases inputs<-
setMethod(f = "inputs<-",signature = c("webprocess",'missing'),
          definition = function(.Object, ..., value){

            args <- expand.grid(..., stringsAsFactors = FALSE)
            for (i in seq_len(ncol(args))){
              .Object <- .setInput(.Object, names(args)[i], args[[i]])
            }
            return(.Object)

          })

#'@rdname inputs-webprocess
#'@aliases inputs<-
setMethod(f = "inputs<-",signature = c("webprocess",'character'),
          definition = function(.Object, ..., value){
            name <- as.character(expand.grid(..., stringsAsFactors = FALSE))
            .Object <- .setInput(.Object, name, value)
            return(.Object)
            
          })

#'@rdname inputs-webprocess
#'@aliases inputs<-
setMethod(f = "inputs<-",signature = c("webprocess",'list'),
          definition = function(.Object, ..., value){

            for (i in seq_len(length(value))){
              .Object <- .setInput(.Object, names(value)[i], value[[i]])
            }
            inputs(.Object, ...)
            return(.Object)
          })

.setInput <- function(.Object, name, arg){
  
  if (is.logical(arg)){
    arg <- ifelse(isTRUE(arg),'true','false')
  }
  
  if (!is.character(arg))
    stop('Process inputs values must be characters.')
  if (!name %in% names(.Object@processInputs))
    stop(name,' not found in ', paste(c('url', 'algorithm', 'version', 'email', 'wait', names(inputs(.Object))), collapse = ', '), call. = FALSE)
  .Object@processInputs[[name]] <- arg
  return(.Object)
}