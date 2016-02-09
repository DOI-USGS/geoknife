
.setProcessInputs <- function(webprocess, ...){
  processNames <- names(inputs(webprocess))
  for (i in seq_along(processNames)){
    fun <- paste0('.',processNames[i])
    if (exists(fun)){
      inputs(webprocess,processNames[i]) <- do.call(fun, list(...))
    } else if (is.null(inputs(webprocess,processNames[i])[[1]])){
      inputs(webprocess,processNames[i]) <- .defaultWhenNull(processNames[i])
    } else {
      # / skip. will skip and allow NA, which is an optional input.
    }
    
  }
  
  return(webprocess)
}

.defaultWhenNull <- function(varName){
  defaults <- list(GROUP_BY = 'STATISTIC',
                   STATISTICS = 'MEAN')
  return(defaults[[varName]])
  
}


.FEATURE_ATTRIBUTE_NAME <- function(stencil,...){
  if (is(stencil,'webgeom')){
    filterBy <- stencil@attribute
  } else if (is(stencil,'simplegeom')){
    filterBy <- 'ID'
  } else {
    stop('FEATURE_ATTRIBUTE_NAME not supported for class ',class(stencil))
  }
  return(filterBy)
}

.TIME_END <- function(fabric, ...){
  strftime(times(fabric)[2] ,format = "%Y-%m-%dT%H:%M:%S.000Z", tz='UTC')
}
.TIME_START <- function(fabric, ...){
  strftime(times(fabric)[1] ,format = "%Y-%m-%dT%H:%M:%S.000Z", tz='UTC')
}

.DATASET_ID <- function(fabric, ...){
  if(!is.na(variables(fabric)[1])) {
    return(variables(fabric))
  } else {
    stop('variables cannot be NA')
  }
}

.DATASET_URI <- function(fabric, ...){
  dodsReplace(url(fabric))
}

