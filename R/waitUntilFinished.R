
#' @rdname wait
#' @aliases wait
#' @title hold up R while GDP is processing
#' 
#' @description keeps R in a loop while GDP works on the request. Checks \code{\link{running}}. 
#' Will drop out of loop whenever !running(geojob)
#' 
#' @param .Object a geojob
#' @param ... other arguments passed to methods
#' @return invisible return of .Object, unaltered
#' @importFrom progress progress_bar
#' @examples
#' \dontrun{
#' job <- geoknife(stencil = c(-89,42), fabric = 'prism')
#' 2+2
#' wait(job, show.progress = TRUE)
#' check(job) # should be complete
#' }
#' @export
setGeneric(name = "wait", 
           def = function(.Object, ...) {
             standardGeneric("wait")
           })

#' @param show.progress logical (optional) show progress bar or not
#' @param sleep.time numeric (optional) a number of seconds to wait in 
#' between checking the process
#' @rdname wait
#' @aliases wait
setMethod(f = "wait", 
          signature(.Object = "geojob"), 
          definition = function(.Object, 
                                sleep.time = gconfig('sleep.time'), 
                                show.progress = gconfig("show.progress")){
            wait(id(.Object), 
                 sleep.time = gconfig('sleep.time'), 
                 show.progress = show.progress)
          })

#' @rdname wait
#' @aliases wait
setMethod(f = "wait", 
          signature(.Object = "character"), 
          definition = function(.Object, 
                                sleep.time = gconfig('sleep.time'), 
                                show.progress = gconfig("show.progress")){
            
            running <- running(.Object, retry = TRUE)
            
            if(show.progress) { 
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
    
                if(show.progress) pb$update(as.numeric(percentComplete)/100)
    
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