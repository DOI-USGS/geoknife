#'@title inputs of webprocess
#'@description access or set the inputs of a webprocess
#'@param .Object a webprocessing object
#'@param value a field pair for .Object processInputs
#'@param ... arguments matching fields in .Object's processInputs slot
#'@rdname inputs-webprocess
#'@aliases
#'inputs
#'inputs<-
#'@examples
#'wp <- webprocess(DELIMITER = 'TAB')
#'inputs(wp)
#'
#'inputs(wp) <- list(DELIMITER = 'COMMA', SUMMARIZE_FEATURE_ATTRIBUTE = 'false')
#'inputs(wp)
#'inputs(wp, "DELIMITER")
#'@export
#'@keywords internal
setGeneric(name="inputs<-",def=function(.Object, value, ...){
  standardGeneric("inputs<-")
})

#'@rdname inputs-webprocess
#'@aliases inputs
#'@export
#'@keywords internal
setGeneric(name="inputs",def=function(.Object, ...){standardGeneric("inputs")})

#'@rdname inputs-webprocess
#'@aliases inputs
setMethod(f = "inputs",signature = "webprocess",
          definition = function(.Object, ...){
            if (missing(...)){
              return(.Object@processInputs)
            } else {
              ret_idx <- which(names(.Object@processInputs) %in% as.character(expand.grid(...,stringsAsFactors = F)))
              return(.Object@processInputs[ret_idx])
            }
          })


#'@rdname inputs-webprocess
#'@aliases inputs
setMethod(f = "inputs<-",signature = c("webprocess",'missing'),
          definition = function(.Object, value, ...){

            args <- expand.grid(..., stringsAsFactors = FALSE)
            for (i in seq_len(ncol(args))){
              .Object <- .setInput(.Object, names(args)[i], args[[i]])
            }
            return(.Object)

          })

#'@rdname inputs-webprocess
#'@aliases inputs
setMethod(f = "inputs<-",signature = c("webprocess",'character'),
          definition = function(.Object, value, ...){
            if (length(value) > 1)
              stop('character input cannot exceed length 1.')
            name <- as.character(expand.grid(..., stringsAsFactors = FALSE))
            .Object <- .setInput(.Object, name, value)
            return(.Object)
            
          })

#'@rdname inputs-webprocess
#'@aliases inputs
setMethod(f = "inputs<-",signature = c("webprocess",'list'),
          definition = function(.Object, value, ...){

            for (i in seq_len(length(value))){
              .Object <- .setInput(.Object, names(value)[i], value[[i]])
            }
            inputs(.Object, ...)
            return(.Object)
          })

.setInput <- function(.Object, name, arg){
  if (!is.character(arg))
    stop('Process inputs values must be characters.')
  if (!name %in% names(.Object@processInputs))
    stop(name,' not found in ', paste(names(inputs(.Object)), collapse = ', '))
  .Object@processInputs[[name]] <- arg
  return(.Object)
}