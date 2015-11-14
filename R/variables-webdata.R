#' the variables of a webdata object
#' 
#' access or set the variables of a webdata object
#' 
#' @param .Object a \code{\link{webdata}} object
#' @param value a character vector for variables
#' @docType methods
#' @usage
#' variables(.Object)
#' variables(.Object) <- value
#' @rdname variables-webdata
#' @aliases 
#' variables<-
#' variables
#' @export
setGeneric(name="variables",def=function(.Object){standardGeneric("variables")})

#'@rdname variables-webdata
#'@docType methods
#'@export
setGeneric(name="variables<-",def=function(.Object, value){standardGeneric("variables<-")})



#'@rdname variables-webdata
#'@export
setMethod(f = "variables",signature(.Object = "webdata"),
          definition = function(.Object){
            return(.Object@variables)
          })


#'@rdname variables-webdata
#'@export
setMethod(f = "variables<-",signature(.Object= "webdata", value = "ANY"),
          definition = function(.Object, value){
            return(initialize(.Object, variables = as.character(value)))
          })


