#'@title the variables of a webdata object
#'@param .Object a \code{\link{webdata}} object
#'@param value a character vector for variables
#'@docType methods
#'@usage
#'variables(.Object)
#'variables(.Object) <- 'ppt'
#'@rdname variables-webdata
#'@aliases 
#'variables<-,webdata,character
#'variables,webdata
#'webdata
#'@export
setGeneric(name="variables",def=function(.Object){standardGeneric("variables")})

#'@rdname variables-webdata
#'@aliases variables,webdata-method
#'@docType methods
#'@export
setMethod(f = "variables",signature(.Object = "webdata"),
          definition = function(.Object){
            return(.Object@variables)
          })

#'@export
setGeneric(name="variables<-",def=function(.Object, value){standardGeneric("variables<-")})

#'@rdname variables-webdata
#'@docType methods
#'@export
setMethod(f = "variables<-",signature(.Object= "webdata", value = "character"),
          definition = function(.Object, value){
            return(initialize(.Object, variables = value))
          })


