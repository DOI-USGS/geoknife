#'@title inputs of webprocess
#'@description access or set the inputs of a webprocess
#'@rdname inputs-webprocess
#'@aliases
#'inputs
#'inputs<-
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
            return(.Object@processInputs)
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
              .Object@processInputs[[names(args)[i]]] <- args[[i]]
            }
            return(.Object)
          })
