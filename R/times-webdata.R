#'@title the times of an webdata object
#'@description Functions to get or set the times of a \code{\link{webdata}} object
#'@param .Object a \code{\link{webdata}} object
#'@param value a POSIXct vector
#'@examples
#'wd <- webdata('prism')
#'times(wd) <- as.POSIXct(c("2012-11-04", "2012-11-12"))
#'times(wd)[1] <- as.POSIXct("2012-11-04")
#'times(wd)
#'@usage
#'times(.Object) <- value
#'times(.Object)
#'@rdname times-webdata
#'@aliases 
#'times
#'times<-
#'@export
setGeneric(name="times",def=function(.Object){standardGeneric("times")})

#'@aliases times
#'@export
setGeneric(name="times<-",def=function(.Object, value){standardGeneric("times<-")})

#'@aliases times
#'@export
setMethod(f = "times<-",signature = "webdata",
          definition = function(.Object, value){
            if (length(value) != 2){
              stop('times input must be a POSIXct vector of length 2')
            }
            
            if (!any(is.na(value)) && value[1] > value[2]){
              stop('time start must proceed time stop in "times" slot for webdata')
            }
            .Object@times <- value
            return(.Object)
          })


#'@aliases times
#'@export
setMethod(f = "times",signature = "webdata",
          definition = function(.Object){
            return(.Object@times)
          })



