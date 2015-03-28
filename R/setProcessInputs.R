
.setProcessInputs <- function(webprocess, ...){
  processNames <- names(webprocess@processInputs)
  for (i in 1:length(processNames)){
    fun <- processNames[i]
    if (exists(fun)){
      webprocess@processInputs[[fun]] <- do.call(fun, list(...))
    } else {
      # / skip
    }
    
  }
  
  return(webprocess)
}



FEATURE_ATTRIBUTE_NAME <- function(stencil,...){
  if (is(stencil,'webgeom')){
    filterBy <- stencil@attribute
  } else if (is(stencil,'simplegeom')){
    filterBy <- 'ID'
  } else {
    stop('FEATURE_ATTRIBUTE_NAME not supported for class ',class(stencil))
  }
  return(filterBy)
}

TIME_END <- function(fabric, ...){
  warning("not yet setting to null if not used")
  strftime(times(fabric)[2] ,format = "%Y-%m-%dT%H:%M:%S.000Z")
}
TIME_START <- function(fabric, ...){
  warning("not yet setting to null if not used")
  strftime(times(fabric)[1] ,format = "%Y-%m-%dT%H:%M:%S.000Z")
}

DATASET_ID <- function(fabric, ...){
  warning("not yet setting to null if not used")
  variables(fabric)
}

DATASET_URI <- function(fabric, ...){
  url(fabric)
}

