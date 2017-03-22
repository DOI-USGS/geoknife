
#' @rdname wait
#' @aliases wait
#' @title hold up R while GDP is processing
#' 
#' @description keeps R in a loop while GDP works on the request. Checks \code{\link{running}}. 
#' Will drop out of loop whenever !running(geojob)
#' 
#' @param .Object a geojob
#' @param sleep.time a number of seconds to wait in between checking the process
#' @return invisible return of .Object, unaltered
#' @examples
#' \dontrun{
#' job <- geoknife(stencil = c(-89,42), fabric = 'prism')
#' 2+2
#' wait(job)
#' check(job) # should be complete
#' }
#' @export
setGeneric(name="wait",def=function(.Object, sleep.time){standardGeneric("wait")})

#' @rdname wait
#' @aliases wait
setMethod(f = "wait",signature(.Object = "geojob", sleep.time = "numeric"), definition = function(.Object, sleep.time){
  wait(id(.Object), sleep.time = sleep.time)
})

#' @rdname wait
#' @aliases wait
setMethod(f = "wait",signature(.Object = "geojob", sleep.time = "missing"), definition = function(.Object, sleep.time){
  wait(id(.Object), sleep.time = gconfig('sleep.time'))
})

#' @rdname wait
#' @aliases wait
setMethod(f = "wait",signature(.Object = "character", sleep.time = "numeric"), definition = function(.Object, sleep.time){
  running <- running(.Object, retry = TRUE)
  while(running){
    Sys.sleep(sleep.time)
    running <- running(.Object, retry = TRUE)
  }
  invisible(.Object)
})

#' @rdname wait
#' @aliases wait
setMethod(f = "wait",signature(.Object = "character", sleep.time = "missing"), definition = function(.Object, sleep.time){
  wait(.Object, sleep.time = gconfig('sleep.time'))
})