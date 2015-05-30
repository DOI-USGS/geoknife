#'@title inputs of webprocess
#'@description access or set the inputs of a webprocess
#'@param .Object a webprocessing object
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
setGeneric(name="inputs<-",def=function(.Object, ...){
  standardGeneric("inputs<-")
})

#'@rdname inputs-webprocess
#'@aliases inputs
setGeneric(name="inputs",def=function(.Object, ...){standardGeneric("inputs")})

#'@rdname inputs-webprocess
#'@aliases inputs
#'@export
setMethod(f = "inputs",signature = "webprocess",
          definition = function(.Object, ...){
            browser()
            if (missing(...)){
              return(.Object@processInputs)
            } else {
              ret_idx <- which(names(.Object@processInputs) %in% as.character(expand.grid(...,stringsAsFactors = F)))
              return(.Object@processInputs[ret_idx])
            }
          })


#'@rdname inputs-webprocess
#'@aliases inputs
#'@export
setMethod(f = "inputs<-",signature = "webprocess",
          definition = function(.Object,...){

            args <- expand.grid(..., stringsAsFactors = FALSE)
            for (i in seq_len(ncol(args))){
              if (!is.character(args[[i]]))
                stop('Process inputs values must be characters.')
              if (!names(args)[i] %in% names(.Object@processInputs))
                stop(names(args)[i],' not found in ', paste(names(inputs(.Object)), collapse = ', '))
              .Object@processInputs[[names(args)[i]]] <- args[[i]]
            }
            return(.Object)
          })
