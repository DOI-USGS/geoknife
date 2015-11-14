#' the algorithm of a webprocess object
#' 
#' Functions to get or set the algorithm of a \linkS4class{webprocess} object. 
#' The algorithm is the type of process that will be used, and can be accessed 
#' or modified using the algorithm method. 
#' 
#'@param .Object a \linkS4class{webprocess} object
#'@param value a list with name of algorithm and relative url endpoint
#'@examples
#'wp <- webprocess()
#'algorithm(wp)
#'@aliases 
#'algorithm
#'algorithm<-
#'@rdname algorithm-webprocess
#'@export
setGeneric(name="algorithm",def=function(.Object){
  standardGeneric("algorithm")
})

#'@rdname algorithm-webprocess
#'@aliases algorithm
#'@export
setGeneric(name="algorithm<-",def=function(.Object, value){
  standardGeneric("algorithm<-")
})

#' @rdname algorithm-webprocess
#' @aliases algorithm
setMethod(f = "algorithm",signature="webprocess",
          definition = function(.Object){
            return(.Object@algorithm)
          }
)

#'@rdname algorithm-webprocess
#'@aliases algorithm
setMethod(f = "algorithm<-",signature = "webprocess",
          definition = function(.Object,value){
            .Object <- initialize(.Object, algorithm = value)
            return(.Object)
          })