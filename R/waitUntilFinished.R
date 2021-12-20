
#' @rdname wait
#' @aliases wait
#' @title hold up R while GDP is processing
#' 
#' @description keeps R in a loop while GDP works on the request. Checks \code{\link{running}}. 
#' Will drop out of loop whenever !running(geojob)
#' 
#' @param .Object a geojob
#' @param sleep.time a number of seconds to wait in between checking the process
#' @param ... optionally accepts a logical input `progress` to 
#' display progress bar or not. If missing, is derived from \code{\link{gconfig}}.
#' @return invisible return of .Object, unaltered
#' @importFrom progress progress_bar
#' @examples
#' \dontrun{
#' job <- geoknife(stencil = c(-89,42), fabric = 'prism', wait = TRUE, show_progress = TRUE)
#' 2+2
#' wait(job)
#' check(job) # should be complete
#' }
#' @export
setGeneric(name = "wait", 
           def = function(.Object, sleep.time, ...) {
             standardGeneric("wait")
           })

#' @rdname wait
#' @aliases wait
setMethod(f = "wait", 
          signature(.Object = "geojob", sleep.time = "numeric"), 
          definition = function(.Object, sleep.time, ...){
            wait(id(.Object), sleep.time = sleep.time, ...)
          })

#' @rdname wait
#' @aliases wait
setMethod(f = "wait", 
          signature(.Object = "geojob", sleep.time = "missing"), 
          definition = function(.Object, sleep.time, ...){
            wait(id(.Object), sleep.time = gconfig('sleep.time'), ...)
          })

#' @rdname wait
#' @aliases wait
setMethod(f = "wait", 
          signature(.Object = "character", sleep.time = "numeric"), 
          definition = function(.Object, sleep.time, ...){
            
            
            
            running <- running(.Object, retry = TRUE)
            
            if(!exists("show_progress")) {
              show_progress <- gconfig("show.progress")
            }
            
            if(show_progress) { 
              pb <- progress_bar$new(total = 100, clear = FALSE)
            }
            
            currentStatus <- 'unknown'
            succeededYet <- FALSE
            while(running){
              
              Sys.sleep(sleep.time)
              
              running <- running(.Object, retry = TRUE)
              checkResult <- check(.Object)
              
              percentComplete <- checkResult$percentComplete
              
              if(!is.null(percentComplete) && !succeededYet) {
    
                if(show_progress) pb$update(as.numeric(percentComplete)/100)
    
              } else if(checkResult$status != currentStatus) {
                
                message(checkResult$status)
                
              }
              
              currentStatus <- checkResult$status
              if(checkResult$statusType == "ProcessSucceeded" && percentComplete == 100) {
                succeededYet <- TRUE
              }
            }
            invisible(.Object)
          })

#' @rdname wait
#' @aliases wait
setMethod(f = "wait", 
          signature(.Object = "character", sleep.time = "missing"), 
          definition = function(.Object, sleep.time, ...){
            wait(.Object, sleep.time = gconfig('sleep.time'), ...)
          })