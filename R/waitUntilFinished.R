
#' @title hold up R while GDP is processing
#' @description keeps R in a loop while GDP works on the request. 
#' Checks \code{\link{running}}. Will drop out of loop whenever !running(geojob)
#' @keywords internal
#' 
waitUntilFinished <- function(.Object, sleep.time = gconfig('sleep.time')){
  running <- running(.Object, retry = TRUE)
  while(running){
    Sys.sleep(sleep.time)
    running <- running(.Object, retry = TRUE)
  }
}
