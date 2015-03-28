#'@title the algorithm of a webprocess object
#'@description Functions to get or set the algorithm of a \linkS4class{webprocess} object
#'@param .Object a \linkS4class{webprocess} object
#'@param value a list with name of algorithm and relative url endpoint
#'@examples
#'wp <- webprocess()
#'algorithm(wp)
#'@usage
#'algorithm(.Object) <- value
#'algorithm(.Object)
#'@rdname algorithm-webprocess
#'@aliases 
#'algorithm
#'algorithm<-
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

#'@rdname algorithm-webprocess
#'@aliases algorithm
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