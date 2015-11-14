#' @title set job state at package level for outgoing requests
#' @description this function keeps the package from rifling off multiple requests 
#' while another is still pending. Package variable \code{jobState} is set with 
#' direct calls to this private function, or as part of any \code{status} type 
#' method for geojob
#' @keywords internal
#' @param state the state (as a character) to set \code{jobState} to. 
#' @return invisible return of \code{jobState} package variable
setJobState <- function(state = 'none'){
  if (!state %in% c('none',"ProcessStarted",'Process successful',"ProcessFailed",'unknown')){
    state = "ProcessFailed"
  } 
  pkg.env$jobState <- state
  
  invisible(pkg.env$jobState)
}

#' @title get job state 
#' @description convienence function for retrieving package-level \code{jobState}
#' @keywords internal
#' @return \code{jobState} package variable
getJobState <- function(){
  pkg.env$jobState
}

#' can start a new job?
#' 
#' checks package job state and returns a boolean for whether a new job can be started
#' 
#' @return TRUE or FALSE
#' @keywords internal
#' 
canStart <- function(){
  state <- getJobState()

  if (state %in% c('none',"ProcessFailed",'Process successful','unknown')){
    return(TRUE)
  } else {  
    return(FALSE)
  }
    
}