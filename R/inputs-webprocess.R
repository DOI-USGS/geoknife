inputs <- function(.Object, ...){
  
  args <- expand.grid(..., stringsAsFactors = FALSE)
  for (i in 1:length(args)){
    .Object@processInputs[[names(args)[i]]] <- args[[i]]
  }
  return(.Object)
}