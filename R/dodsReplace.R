dodsReplace  <-	function(.Object){
  # checks for dods or opendap, and replaces
  
  
  if ("DATASET_URI" %in% names(.Object@processInputs) & 
        !is.null(.Object@processInputs$DATASET_URI)) {
    
    uri	<-	.Object@processInputs$DATASET_URI
    if (grepl('dodsC',uri)){
      uri	<-	gsub('http', 'dods', uri)
    }
    if (grepl('opendap',uri)){
      uri	<-	gsub('http', 'opendap', uri)
    }
    
    .Object@processInputs$DATASET_URI	<-	uri
  }
  return(.Object)
}