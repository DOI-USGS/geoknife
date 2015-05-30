inputs <- function(.Object, ...){
  
  args <- expand.grid(..., stringsAsFactors = FALSE)
  for (i in seq_len(nrow(args))){
    .Object@processInputs[[names(args)[i]]] <- args[[i]]
  }
  return(.Object)
}