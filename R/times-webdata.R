#' the times of an webdata object
#'
#' Functions to get or set the times of a \code{\link{webdata}} object
#' 
#' @param .Object a \code{\link{webdata}} object
#' @param value a POSIXct vector
#' @examples
#' wd <- webdata('prism')
#' times(wd) <- as.POSIXct(c("2012-11-04", "2012-11-12"))
#' times(wd)[1] <- as.POSIXct("2012-11-04")
#' times(wd)
#' @rdname times-webdata
#' @aliases 
#' times
#' times<-
#' @export
setGeneric(name="times",def=function(.Object){standardGeneric("times")})

#' @rdname times-webdata
#' @aliases times
#' @export
setGeneric(name="times<-",def=function(.Object, value){standardGeneric("times<-")})

#' @rdname times-webdata
#' @aliases times
setMethod(f = "times<-",signature(.Object = "webdata"),
          definition = function(.Object, value){
            if (length(value) != 2){
              stop('times input must be a POSIXct vector of length 2')
            }
            
            if (!any(is.na(value)) && value[1] > value[2]){
              stop('time start must proceed time stop in "times" slot for webdata', call. = FALSE)
            }
            .Object@times <- geotime(value)
            return(.Object)
          })

geotime <- function(value){

  if (is.character(value)){
    geotime.character(value)
  } else if ("POSIXct" %in% class(value)){
    geotime.POSIXct(value)
  } else {
    warning('no applicable geotime method for ',class(value))
  }
  
}

geotime.POSIXct = function(value){
  
  tz = "UTC"
  
  # check for tz, honor if it exists
  if (is.null(attr(value,'tzone')) ||  attr(value,'tzone') == ''){
    as.POSIXct(format(value,usetz=F), tz=tz)
  } else {
    attr(value,'tzone') <- tz
    return(value)
  }
}

geotime.character = function(value){
  geotime.POSIXct(do.call(c, lapply(value,as.POSIXct)))
}

#' @rdname times-webdata
#' @aliases times
setMethod(f = "times",signature(.Object = "webdata"),
          definition = function(.Object){
            return(.Object@times)
          })



